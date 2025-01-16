"""
A module for building HTML/XML documents using context managers.
"""

import functools
import xml.etree.ElementTree as ET

from io import StringIO
from typing import (
    Any,
    Callable,
)
from contextlib import contextmanager
from contextvars import ContextVar

from fastapi import (
    Response,
    Request,
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
        parent = node.get()
        parent.append(element)

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
    else:
        element.text = (element.text or "") + content


def attr(name: str, value: str | bool | None = True):
    element = node.get()
    xml_name = attr_name_to_xml(name)

    if value is None or value is False:
        element.attrib.pop(xml_name, None)
    elif value is True:
        element.set(xml_name, "")
    else:
        element.set(xml_name, str(value))


def classes(*names: str):
    current = node.get().get("class", "").strip()
    if current and names:
        current += " "
    node.get().set("class", current + " ".join(names))


def dataset(data: dict[str, str]):
    for k, v in data.items():
        attr(f"data-{k}", v)


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
