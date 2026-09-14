"""
Block-oriented HTML/XML generation with context managers, plus live regions
that a server can re-render and push to the browser over a WebSocket.
"""

import random
import logging
import pathlib
import functools
import xml.etree.ElementTree as ET
import re

from io import StringIO
from dataclasses import dataclass, field, asdict
from typing import (
    Any,
    TYPE_CHECKING,
    List,
    Union,
    Literal,
    Callable,
    Optional,
)
from contextlib import contextmanager, asynccontextmanager
from contextvars import ContextVar

import anyio
import anyio.abc
from anyio.abc import TaskGroup

# Import Starlette (base framework used by FastAPI)
try:
    from starlette.responses import Response, HTMLResponse
    from starlette.requests import Request
    from starlette.websockets import WebSocket, WebSocketDisconnect
    from starlette.staticfiles import StaticFiles
    from starlette.applications import Starlette
    from starlette.middleware.base import (
        BaseHTTPMiddleware,
        RequestResponseEndpoint,
    )

    HAS_STARLETTE = True
except ImportError:
    HAS_STARLETTE = False
    # Keep standalone rendering available without optional web dependencies.
    # Type check integrations against the real framework classes above.
    if TYPE_CHECKING:
        raise
    else:

        class Response:
            """Standalone response renderer, not an ASGI response."""

            media_type = "text/html"

            def __init__(self, content=None, **kwargs):
                self.content = content
                for key, value in kwargs.items():
                    setattr(self, key, value)

            def render(self, content):
                return str(content).encode("utf-8") if content else b""

        class HTMLResponse(Response):
            """Standalone HTML response renderer."""

            media_type = "text/html"

        class BaseHTTPMiddleware:
            """Placeholder when Starlette is not installed."""

        Request = object
        RequestResponseEndpoint = object
        WebSocket = object
        WebSocketDisconnect = Exception
        StaticFiles = object
        Starlette = object

logger = logging.getLogger(__name__)


def mint() -> str:
    """Generate a concise unique ID using base62 encoding (A-Z, a-z, 0-9)."""
    # Use 48 bits of randomness encoded in base62
    # This gives us ~281 trillion unique IDs
    n = random.getrandbits(48)
    chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    result = ""
    while n:
        n, remainder = divmod(n, 62)
        result = chars[remainder] + result
    # Pad to ensure consistent length
    return result.zfill(8)


# -----------------------------------------------------------------------------
# 1. Fragment: The root of a Tagflow context
# -----------------------------------------------------------------------------


class Fragment:
    """
    Represents a collection of HTML/XML elements that can be rendered
    as a complete document or XML fragment.
    """

    def __init__(self):
        self.element = ET.Element("fragment")

    def __str__(self) -> str:
        return self.to_html()

    def to_html(self, compact: bool = True) -> str:
        """
        Renders the fragment as HTML. By default, it concatenates the
        top-level elements without indentation or line breaks.
        """
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
        """
        Renders the fragment as XML. If there's more than one top-level
        element, we raise an error, since XML requires a single root.
        """
        if len(self.element) > 1:
            raise ValueError("Fragment has more than one root element.")

        tree = ET.ElementTree(self.element[0])
        s = StringIO()
        tree.write(s, encoding="unicode", method="xml")
        return s.getvalue()


# -----------------------------------------------------------------------------
# 2. Context Variables
# -----------------------------------------------------------------------------

# The current node in the document to which we are appending content
node: ContextVar[ET.Element] = ContextVar("node")

# The current document fragment (root)
root_fragment: ContextVar[Fragment] = ContextVar("root")


@contextmanager
def enter(element: ET.Element):
    """
    Context manager for entering an element.
    """
    token = node.set(element)
    try:
        yield element
    finally:
        node.reset(token)


# -----------------------------------------------------------------------------
# 3. Public API Context Managers
# -----------------------------------------------------------------------------


@contextmanager
def document():
    """
    Creates a new document context for building HTML/XML content. The
    returned value is a Fragment, which can be rendered to HTML or XML.
    """
    doc = Fragment()
    token_root = root_fragment.set(doc)
    token_node = node.set(doc.element)
    try:
        yield doc
    finally:
        root_fragment.reset(token_root)
        node.reset(token_node)


# -----------------------------------------------------------------------------
# 4. Core Tag Building
# -----------------------------------------------------------------------------


def attr_name_to_xml(name: str) -> str:
    """
    Convert Pythonic attribute names to valid HTML/XML attribute names.
    If 'classes' or 'class_' is passed in, that maps to the 'class' attribute.
    Otherwise, replace underscores between word characters with hyphens.
    """
    if name == "classes" or name == "class_":
        return "class"
    return re.sub(r"(?<=\w)_(?=\w)", "-", name)


# Type for class names that can be arbitrarily nested lists of strings
ClassValue = Union[str, None, List["ClassValue"]]

# Type for any HTML attribute value
AttrValue = Union[str, int, float, bool, ClassValue]


def attr_value_to_str(value: AttrValue, attr_name: str) -> str:
    """Convert an attribute value to its string representation for HTML output."""
    if value is True:
        return ""
    if isinstance(value, (str, list)):
        return strs(value)
    if isinstance(value, (int, float)):
        return str(value)
    raise TypeError(
        f"Attribute values must be strings, numbers, booleans, or lists. "
        f"Got {type(value)} for attribute '{attr_name}'"
    )


def strs(value: Union[str, ClassValue]) -> str:
    """
    Helper to convert a string or nested list of strings to a single space-
    separated string. Handles arbitrarily nested lists of strings.
    """
    if isinstance(value, str):
        return value
    elif isinstance(value, list):
        return " ".join(strs(v) for v in value if v)
    return ""


class HTMLTagBuilder:
    """
    Provides a convenient API for creating HTML elements:
      with tag.div(class="container"):
          ...
    or:
      with tag("div", id="something"):
          ...
    """

    def __call__(
        self,
        tagname: str,
        *klasses: ClassValue,
        **kwargs: AttrValue,
    ):
        """
        Creates a new HTML/XML element with the given tag name and
        attributes. Returns a context manager for adding child elements.
        """
        # Convert kwargs to element attributes
        attrs = {}
        for k, v in kwargs.items():
            if v is None or v is False:
                # skip falsey attributes
                continue
            xml_name = attr_name_to_xml(k)
            attrs[xml_name] = attr_value_to_str(v, k)

        # Now merge any klasses with the class attribute if present
        if klasses:
            class_attr = attrs.get("class")
            class_values = list(klasses)
            if class_attr:
                class_values.append(class_attr)
            attrs["class"] = strs(class_values)

        element = ET.Element(tagname, attrib=attrs)
        node.get().append(element)
        return enter(element)

    def __getattr__(self, name: str) -> Callable[..., Any]:
        """
        Fallback for dot-access style creation:
            tag.div(id="test")  -> tag("div", id="test")
        """
        return lambda *args, **kw: self.__call__(name, *args, **kw)


tag = HTMLTagBuilder()


def tag_decorator(tag_name: str, *klasses: ClassValue, **kwargs: AttrValue):
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs2):
            with tag(tag_name, *klasses, **kwargs):
                return func(*args, **kwargs2)

        return wrapper

    return decorator


class HTMLDecorators:
    """
    Provides a convenient API for creating HTML elements as decorators.
    Usage: @html.div(class="container") or @html("div", class="container")
    """

    def __getattr__(self, name: str) -> Callable[..., Any]:
        return lambda *args, **kwargs: tag_decorator(name, *args, **kwargs)

    def __call__(
        self, name: str, *klasses: ClassValue, **kwargs: AttrValue
    ) -> Callable[[Any], Any]:
        return tag_decorator(name, *klasses, **kwargs)


html = HTMLDecorators()


# -----------------------------------------------------------------------------
# 5. Convenience Functions
# -----------------------------------------------------------------------------


def text(content: str):
    """
    Appends text to the current element. If the current element already
    has children, the text is appended to the tail of the last child.
    """
    current_el = node.get()
    if len(current_el) > 0:
        last_child = current_el[-1]
        last_child.tail = (last_child.tail or "") + content
    else:
        current_el.text = (current_el.text or "") + content


def attr(name: str, value: AttrValue):
    """
    Sets or removes an attribute on the current element. If `value`
    is None or False, the attribute is removed. If `value` is True,
    the empty string is used.
    """
    current_el = node.get()
    xml_name = attr_name_to_xml(name)

    if value is None or value is False:
        if xml_name in current_el.attrib:
            current_el.attrib.pop(xml_name)
    elif value is True:
        current_el.set(xml_name, "")
    else:
        current_el.set(xml_name, attr_value_to_str(value, name))


def classes(*names: ClassValue):
    """
    Appends the given class names to the current element's 'class' attribute.
    Handles arbitrarily nested lists of strings.
    """
    el = node.get()
    current_classes = el.get("class", "").strip()
    if current_classes and names:
        current_classes += " "
    el.set("class", current_classes + strs(list(names)))


def dataset(data: dict[str, str]):
    """
    Sets data-* attributes from a dict. E.g. dataset({"foo": "bar"})
    sets the attribute data-foo="bar".
    """
    for k, v in data.items():
        attr(f"data-{k}", v)


def clear():
    """
    Removes all children and text of the current element. The tail belongs
    to the parent, not to this element's contents, so it is left alone.
    """
    current_el = node.get()
    del current_el[:]
    current_el.text = None


# -----------------------------------------------------------------------------
# 6. Rendering Helpers
# -----------------------------------------------------------------------------


def render(component: Callable[[], None]) -> str:
    """
    Run `component` inside a fresh document and return the HTML it built.
    This is the explicit counterpart to the ambient `document()` context:
    nothing leaks into or out of the surrounding document, if any.
    """
    with document() as doc:
        component()
    return doc.to_html()


@dataclass(frozen=True)
class Region:
    """
    The rendered form of a region: one element that owns its identity.
    `html` is the element's outer HTML and `id` is its `id` attribute.
    """

    id: str
    html: str


def render_region(component: Callable[[], None]) -> Region:
    """
    Render `component` as a region: it must produce exactly one root element
    carrying an `id`, because that id is how the browser finds the element
    to morph, whether the new HTML arrives by an htmx request the element
    made for itself or by a push from a live `Session`.
    """
    with document() as doc:
        component()
    roots = list(doc.element)
    if len(roots) != 1:
        raise ValueError(
            f"A region must render exactly one root element, "
            f"got {len(roots)}"
        )
    region_id = roots[0].get("id")
    if not region_id:
        raise ValueError(
            f"A region's root <{roots[0].tag}> must have an id attribute"
        )
    return Region(region_id, doc.to_html())


def document_html() -> str:
    """
    Returns the entire document as an HTML string, prefixed by the
    doctype declaration.
    """
    doc = root_fragment.get(None)
    if not doc:
        return "<!doctype html><html><body>Error: No root document</body></html>"
    return f"<!doctype html>\n{doc.to_html()}"


# -----------------------------------------------------------------------------
# 7. Response Classes
# -----------------------------------------------------------------------------


class TagResponse(HTMLResponse):
    """
    A FastAPI-compatible response class that captures the Tagflow
    document context and renders it as HTML.
    """

    def render(self, content: Optional[str] = None) -> bytes:
        doc = root_fragment.get(None)
        if doc is not None:
            return document_html().encode("utf-8")
        else:
            # If not in a Tagflow context, fallback
            return bytes(super().render(content or ""))


class XMLResponse(Response):
    """
    A FastAPI-compatible response class that captures the Tagflow
    document context and renders it as XML.
    """

    media_type = "application/xml"

    def render(self, content: Any) -> bytes:
        doc = root_fragment.get(None)
        if doc is not None:
            return doc.to_xml().encode("utf-8")
        else:
            return str(content).encode("utf-8")


# -----------------------------------------------------------------------------
# 8. Document Middleware
# -----------------------------------------------------------------------------


class DocumentMiddleware(BaseHTTPMiddleware):
    """
    Middleware that sets up a fresh document context for each request.

    Usage in FastAPI:
        app = FastAPI()
        app.add_middleware(DocumentMiddleware)
    """

    async def dispatch(
        self, request: Request, call_next: RequestResponseEndpoint
    ):
        with document():
            response = await call_next(request)
            return response


# -----------------------------------------------------------------------------
# 9. Live Regions
# -----------------------------------------------------------------------------
#
# A live page holds regions the server re-renders and pushes over a
# WebSocket. The unit of change is the same one the htmx integration uses: a
# whole element with an id, morphed in place. The server keeps only the
# latest HTML of each region, so a connection that arrives late or
# reconnects converges by receiving whatever it has not seen. Nothing on the
# server mirrors the browser's DOM.


@dataclass
class Morph:
    target: str  # id of the element to morph
    html: str  # its new outer HTML
    type: Literal["morph"] = "morph"


@dataclass
class Update:
    """One message: morphs the browser applies together."""

    morphs: list[Morph]
    type: Literal["update"] = "update"


# WebSocket close code telling the client its session is gone for good.
SESSION_EXPIRED = 4001


@dataclass
class Session:
    """
    The live regions of one rendered page. Create it with `Live.session()`,
    place `client_tag()` in the page outside any region, then call
    `update()` from any task whenever a region's state changes.
    """

    id: str
    taskgroup: TaskGroup
    grace: float
    regions: dict[str, str] = field(default_factory=dict)
    connections: int = 0
    changed: anyio.Event = field(default_factory=anyio.Event)
    closed: anyio.Event = field(default_factory=anyio.Event)
    _unattached_since: float = field(default_factory=anyio.current_time)

    def update(self, *components: Callable[[], None]) -> None:
        """
        Re-render each component as a region and push the results. All the
        regions of one call reach the browser in one message and are applied
        together. Rendering happens first, so a component that raises pushes
        nothing.
        """
        rendered = [render_region(component) for component in components]
        for region in rendered:
            self.regions[region.id] = region.html
        changed, self.changed = self.changed, anyio.Event()
        changed.set()

    def spawn(self, fn: Callable[..., Any]) -> None:
        """Start a task that ends with the session."""
        self.taskgroup.start_soon(fn)

    def cancel(self) -> None:
        """End the session: its tasks stop and its browsers are told."""
        self.taskgroup.cancel_scope.cancel()

    def client_tag(self) -> None:
        """
        Insert the element that connects this page to the session. Place it
        outside every region: a morph replaces a region's contents, and the
        connection must outlive them.
        """
        tag("tagflow-client", session_id=self.id)

    def attach(self) -> None:
        self.connections += 1

    def detach(self) -> None:
        self.connections -= 1
        if self.connections == 0:
            self._unattached_since = anyio.current_time()

    async def run(self) -> None:
        """
        Keep the session while a browser is attached. Once none has been for
        at least `grace` seconds (checked every `grace`), end it.
        """
        while True:
            await anyio.sleep(self.grace)
            idle = anyio.current_time() - self._unattached_since
            if self.connections == 0 and idle >= self.grace:
                self.cancel()


class Live:
    """
    Serves the WebSocket and client script for live regions.

        live = Live()
        app = Starlette(lifespan=live.run)

    The client script is mounted under `/.well-known/tagflow/static/` and
    the WebSocket at `/.well-known/tagflow/live.ws`.
    """

    STATIC = "/.well-known/tagflow/static"
    SOCKET = "/.well-known/tagflow/live.ws"

    def __init__(self, *, grace: float = 30.0):
        self.grace = grace
        self._taskgroup: Optional[TaskGroup] = None
        self._sessions: dict[str, Session] = {}

    @asynccontextmanager
    async def run(self, app: Starlette):
        """Lifespan: mount the routes and own every session's tasks."""
        async with anyio.create_task_group() as taskgroup:
            self._taskgroup = taskgroup
            app.mount(
                self.STATIC,
                StaticFiles(
                    directory=str(pathlib.Path(__file__).parent / "static")
                ),
                name="tagflow_static",
            )
            app.router.add_websocket_route(
                self.SOCKET, self.handle_websocket
            )
            try:
                yield
            finally:
                # Sessions run until cancelled; shutdown must cancel them.
                self._taskgroup = None
                taskgroup.cancel_scope.cancel()

    async def session(self) -> Session:
        """Create a session whose tasks live in the `Live` task group."""
        if not self._taskgroup:
            raise RuntimeError(
                "Live.run() must be called before creating a session."
            )
        return await self._taskgroup.start(self._run_session, mint())

    async def _run_session(
        self,
        session_id: str,
        *,
        task_status: "anyio.abc.TaskStatus[Session]",
    ) -> None:
        async with anyio.create_task_group() as taskgroup:
            session = Session(session_id, taskgroup, self.grace)
            self._sessions[session_id] = session
            try:
                logger.info("Session %s running", session_id)
                task_status.started(session)
                await session.run()
            finally:
                logger.info("Session %s ended", session_id)
                del self._sessions[session_id]
                session.closed.set()

    def script_tag(self) -> None:
        """Insert the client scripts: the morph algorithm and the client."""
        for name in ("idiomorph.min.js", "tagflow.js"):
            tag.script(src=f"{self.STATIC}/{name}", defer=True)

    async def handle_websocket(self, websocket: WebSocket) -> None:
        """
        The client's first message names its session. From then on the
        connection receives every region it has not yet seen at its latest
        HTML, as one update per change, until the session ends.
        """
        await websocket.accept()
        hello = await websocket.receive_json()
        session_id = hello.get("id") if isinstance(hello, dict) else None
        session = (
            self._sessions.get(session_id)
            if isinstance(session_id, str)
            else None
        )
        if session is None:
            await websocket.close(
                code=SESSION_EXPIRED, reason="unknown session"
            )
            return

        async def push() -> None:
            seen: dict[str, str] = {}
            while True:
                changed = session.changed
                morphs = [
                    Morph(target, html)
                    for target, html in session.regions.items()
                    if seen.get(target) != html
                ]
                if morphs:
                    await websocket.send_json(asdict(Update(morphs)))
                    seen.update((m.target, m.html) for m in morphs)
                await changed.wait()

        async def expire() -> None:
            await session.closed.wait()
            await websocket.close(
                code=SESSION_EXPIRED, reason="session ended"
            )

        session.attach()
        try:
            async with anyio.create_task_group() as connection:
                connection.start_soon(push)
                connection.start_soon(expire)
                try:
                    while True:
                        await websocket.receive_json()
                except WebSocketDisconnect:
                    pass
                connection.cancel_scope.cancel()
        finally:
            session.detach()
