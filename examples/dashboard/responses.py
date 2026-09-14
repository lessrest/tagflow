"""Render a Tagflow component into an HTTP representation.

``render_response`` is the explicit boundary between a component callable and
a Starlette response: render once inside an isolated document scope, produce
the exact bytes, and apply conditional-request semantics to those bytes. It
does not depend on request middleware, ambient document state, or Tagflow's
WebSocket sessions. Cache policy is a required argument on purpose: nothing
here decides that a representation is public or private.

This is a cookbook prototype for a possible ``tagflow.starlette`` helper.
"""

import re

from hashlib import sha256
from collections.abc import Callable, Mapping

from starlette.requests import Request
from starlette.responses import HTMLResponse, Response

from tagflow import document

# RFC 9110 entity-tag: optional weakness indicator and a quoted opaque tag.
ENTITY_TAG = re.compile(r'(W/)?"([^"]*)"')


def render(component: Callable[[], None]) -> str:
    """Render ``component`` in a fresh document and return its HTML."""
    with document() as doc:
        component()
    return doc.to_html()


def etag_for(body: bytes) -> str:
    """A strong validator derived from the representation bytes themselves."""
    return '"' + sha256(body).hexdigest() + '"'


def matches(if_none_match: str, etag: str) -> bool:
    """Weak comparison of an ``If-None-Match`` field against ``etag``.

    The weakness indicator is ignored on both sides, as required for
    If-None-Match, and ``*`` matches any current representation.
    """
    if if_none_match.strip() == "*":
        return True
    opaque = ENTITY_TAG.fullmatch(etag)
    assert opaque is not None, "etag must be a quoted entity-tag"
    return any(
        candidate == opaque.group(2)
        for _, candidate in ENTITY_TAG.findall(if_none_match)
    )


def render_response(
    request: Request,
    component: Callable[[], None],
    *,
    cache_control: str,
    doctype: bool = False,
    conditional: bool = True,
    headers: Mapping[str, str] | None = None,
    status_code: int = 200,
) -> Response:
    """Render ``component`` and return it as an HTML response.

    With ``conditional``, the response carries a strong ETag computed from
    the rendered bytes, and a GET or HEAD whose ``If-None-Match`` matches
    receives 304 with the same validator and cache headers. Rendering still
    happens before hashing; this trades render cost for a validator that is
    correct across filters and template changes. Pass ``conditional=False``
    with ``cache_control="no-store"`` for error or recovery representations
    that must never be revalidated against an old reader.
    """
    body = ("<!doctype html>\n" if doctype else "") + render(component)
    payload = body.encode("utf-8")
    response_headers = {"Cache-Control": cache_control, **(headers or {})}
    if conditional:
        etag = etag_for(payload)
        response_headers["ETag"] = etag
        if request.method in ("GET", "HEAD") and matches(
            request.headers.get("if-none-match", ""), etag
        ):
            return Response(status_code=304, headers=response_headers)
    return HTMLResponse(
        payload, status_code=status_code, headers=response_headers
    )
