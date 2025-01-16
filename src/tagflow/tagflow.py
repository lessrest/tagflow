"""
A module for building HTML/XML documents using context managers.
"""

import uuid
import functools
import xml.etree.ElementTree as ET

from io import StringIO
from typing import (
    Any,
    Literal,
    Callable,
    TypedDict,
)
from contextlib import contextmanager, asynccontextmanager
from contextvars import ContextVar
from dataclasses import dataclass

import trio

from fastapi import (
    FastAPI,
    Request,
    Response,
    WebSocket,
)
from fastapi.responses import HTMLResponse
from starlette.middleware.base import (
    BaseHTTPMiddleware,
    RequestResponseEndpoint,
)


def element(tag_name: str, *klasses: str, **kwargs):
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs2):
            with tag(tag_name, **kwargs):
                classes(*klasses)
                return func(*args, **kwargs2)

        return wrapper

    return decorator


class Fragment:
    """
    Represents a collection of HTML/XML elements that can be rendered
    as a complete document or XML fragment.
    """

    live: bool = False

    def __init__(self):
        self.element = ET.Element("fragment")

    def __str__(self) -> str:
        return self.to_html()

    def to_html(self, compact: bool = True) -> str:
        if len(self.element) == 0:
            return ""
        elif len(self.element) > 1 and not compact:
            raise ValueError(
                "Pretty printing requires exactly one root element"
            )

        if compact:
            return "".join(
                ET.tostring(child, encoding="unicode", method="html")
                for child in self.element
            )

        # For pretty printing, use BeautifulSoup
        from bs4 import BeautifulSoup

        element = (
            self.element[0] if len(self.element) == 1 else self.element
        )
        rough_string = ET.tostring(
            element, encoding="unicode", method="html"
        )
        soup = BeautifulSoup(rough_string, "html.parser")
        return soup.prettify()

    def to_xml(self) -> str:
        if len(self.element) > 1:
            raise ValueError("Fragment has more than one element")
        tree = ET.ElementTree(self.element[0])
        s = StringIO()
        tree.write(s, encoding="unicode", method="xml")
        return s.getvalue()


def attr_name_to_xml(name: str) -> str:
    if name == "classes":
        return "class"
    # replace _ with -
    return name.replace("_", "-").removesuffix("-")


# Context variables to track the current element and root document
node: ContextVar[ET.Element] = ContextVar("node")
root: ContextVar[Fragment] = ContextVar("root")


class MutationEvent(TypedDict):
    type: Literal["append", "setAttribute", "setText", "clear"]
    target: str  # Element ID
    name: str | None  # For setAttribute
    value: str | None  # For setAttribute/setText/setTail
    child: str | None  # For append (child element ID)


@dataclass
class Transaction:
    mutations: list[MutationEvent]


tx: ContextVar[Transaction | None] = ContextVar("tx", default=None)


def _get_or_create_id(element: ET.Element) -> str:
    """Get or create a unique ID for an element"""
    id = element.get("id")
    if not id and root.get().live:
        id = str(uuid.uuid4())
        element.set("id", id)
    return id


def record_mutation(event: MutationEvent):
    """Record a mutation if there's an active transaction"""
    transaction = tx.get()
    if transaction:
        transaction.mutations.append(event)


@contextmanager
def document():
    """Creates a new document context for building HTML/XML content"""
    doc = Fragment()
    token = root.set(doc)
    token2 = node.set(doc.element)
    try:
        yield doc
    finally:
        root.reset(token)
        node.reset(token2)


def strs(value: str | list[str]) -> str:
    if isinstance(value, list):
        return " ".join(value)
    return value


class HTMLTagBuilder:
    """
    Provides a convenient API for creating HTML elements.
    Usage: with tag.div(...): or with tag("div", ...):
    """

    def __getattr__(self, name: str) -> Callable[..., Any]:
        return lambda **kwargs: self.__call__(name, **kwargs)

    def __call__(self, tagname: str, **kwargs):
        """
        Creates a new HTML/XML element with the given tag name and attributes.
        Returns a context manager for adding child elements.
        """
        element = ET.Element(
            tagname,
            attrib={
                attr_name_to_xml(k): "" if v is True else strs(v)
                for k, v in kwargs.items()
                if v
            },
        )

        if "id" not in kwargs and root.get().live:
            kwargs["id"] = str(uuid.uuid4())

        parent = node.get()
        parent.append(element)

        # Record the append mutation
        record_mutation(
            {
                "type": "append",
                "target": _get_or_create_id(parent),
                "child": _get_or_create_id(element),
                "name": None,
                "value": None,
            }
        )

        @contextmanager
        def context():
            token = node.set(element)
            try:
                yield element
            finally:
                node.reset(token)

        return context()


class HTMLDecorators:
    """
    Provides a convenient API for creating HTML elements as decorators.
    Usage: @html.div(class="container") or @html("div", class="container")
    """

    def __getattr__(self, name: str) -> Callable[..., Any]:
        return lambda *args, **kwargs: element(name, *args, **kwargs)

    def __call__(
        self, name: str, *klasses: str, **kwargs: Any
    ) -> Callable[[Any], Any]:
        return element(name, *klasses, **kwargs)


html = HTMLDecorators()
tag = HTMLTagBuilder()


def text(content: str):
    element = node.get()
    if len(element) > 0:
        last_child = element[-1]
        last_child.tail = (last_child.tail or "") + content
        record_mutation(
            {
                "type": "setText",
                "target": _get_or_create_id(last_child),
                "value": last_child.tail,
                "name": None,
                "child": None,
            }
        )
    else:
        element.text = (element.text or "") + content
        record_mutation(
            {
                "type": "setText",
                "target": _get_or_create_id(element),
                "value": element.text,
                "name": None,
                "child": None,
            }
        )


def attr(name: str, value: str | bool | None = True):
    element = node.get()
    xml_name = attr_name_to_xml(name)

    if value is None or value is False:
        element.attrib.pop(xml_name, None)
    elif value is True:
        element.set(xml_name, "")
    else:
        element.set(xml_name, str(value))

    record_mutation(
        {
            "type": "setAttribute",
            "target": _get_or_create_id(element),
            "name": xml_name,
            "value": element.get(xml_name),
            "child": None,
        }
    )


def classes(*names: str):
    current = node.get().get("class", "").strip()
    if current and names:
        current += " "
    node.get().set("class", current + " ".join(names))


def dataset(data: dict[str, str]):
    for k, v in data.items():
        attr(f"data-{k}", v)


def clear():
    record_mutation(
        {
            "type": "clear",
            "target": _get_or_create_id(node.get()),
        }
    )
    node.get().clear()


def document_html():
    doc = root.get()
    assert doc is not None
    html = doc.to_html()
    return f"<!doctype html>\n{html}"


class TagResponse(HTMLResponse):
    def render(self, content: str | None = None) -> bytes | memoryview:
        doc = root.get()
        if doc:
            return document_html().encode("utf-8")
        else:
            return super().render(content)


class XMLResponse(Response):
    media_type = "application/xml"

    def render(self, content: Any) -> bytes:
        doc = root.get()
        if doc:
            return doc.to_xml().encode("utf-8")
        else:
            return str(content).encode("utf-8")


class DocumentMiddleware(BaseHTTPMiddleware):
    """Middleware that sets up a fresh document context for each request.

    Usage:
        app = FastAPI()
        app.add_middleware(DocumentMiddleware)
    """

    async def dispatch(
        self, request: Request, call_next: RequestResponseEndpoint
    ):
        with document():
            response = await call_next(request)
            return response


@dataclass
class Session:
    """A live session that manages WebSocket connections and updates."""

    id: str
    nursery: trio.Nursery
    send_channel: trio.MemorySendChannel
    receive_channel: trio.MemoryReceiveChannel

    @asynccontextmanager
    async def transition(self):
        """Context manager for atomic document updates."""
        transaction = Transaction(mutations=[])
        token = tx.set(transaction)
        try:
            yield
            # Send update through WebSocket with the collected mutations
            await self.send_channel.send(
                {"type": "update", "mutations": transaction.mutations}
            )
        finally:
            tx.reset(token)

    def spawn(self, fn: Callable) -> None:
        """Spawn a new task in the session's nursery."""
        self.nursery.start_soon(fn)

    async def run(self):
        while True:
            await self.receive_channel.receive()
            # todo


class Future[T]:
    send_channel: trio.MemorySendChannel[T]
    receive_channel: trio.MemoryReceiveChannel[T]

    def __init__(self):
        self.send_channel, self.receive_channel = trio.open_memory_channel(
            0
        )

    async def provide(self, value: T):
        await self.send_channel.send(value)

    async def consume(self) -> T:
        return await self.receive_channel.receive()


@asynccontextmanager
async def future[T]():
    future = Future[T]()
    with future.send_channel, future.receive_channel:
        yield future


class Live:
    """Manages live document sessions and WebSocket connections."""

    def __init__(self):
        self._nursery: trio.Nursery | None = None
        self._sessions: dict[str, Session] = {}

    @asynccontextmanager
    async def run(self, app: FastAPI):
        """Start the live document manager."""
        async with trio.open_nursery() as nursery:
            self._nursery = nursery
            app.websocket("/.well-known/tagflow/live.ws")(
                self.handle_websocket
            )
            yield
            # Nursery will be cancelled on exit

    async def session(self):
        """Create a new live session."""
        if not self._nursery:
            raise RuntimeError("Live.run() must be called first")

        id = uuid.uuid4()
        send_channel, receive_channel = trio.open_memory_channel(0)

        doc = root.get()
        doc.live = True

        async with future() as session_future:

            async def session_task():
                async with trio.open_nursery() as session_nursery:
                    session = Session(
                        id=str(id),
                        nursery=session_nursery,
                        send_channel=send_channel,
                        receive_channel=receive_channel,
                    )
                    self._sessions[str(id)] = session
                    await session_future.provide(session)
                    try:
                        await session.run()
                    finally:
                        del self._sessions[str(id)]

        self._nursery.start_soon(session_task)

    def script_tag(self) -> None:
        """Insert the live document JavaScript code."""
        # TODO: Implement client-side JavaScript injection
        pass

    async def handle_websocket(self, websocket: WebSocket):
        """Handle WebSocket connections for live updates."""
        await websocket.accept()
        hello = await websocket.receive_json()
        id = hello["id"]
        session = self._sessions[id]
        if not session:
            raise RuntimeError(f"Session {id} not found")

        recv = session.receive_channel.clone()
        with recv:
            while True:
                update = await recv.receive()
                await websocket.send_json(update)
