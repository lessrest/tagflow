"""The URL schema: each resource declares its path and what its URLs carry.

The dashboard's worst bugs were consistency failures between three things
that must agree about one resource: the route that serves it, the URL an
element uses to fetch it again, and the query parameters that URL must keep
so the refreshed representation matches the page it sits in (a cursor link
that dropped ``transport`` silently switched a polling reader to SSE). Each
:class:`Resource` states its path template and the :class:`View` fields its
URLs carry, once; ``views.py`` derives every URL from it and ``app.py``
derives every route. There is no library help for this and there should
not be: which parameters a resource depends on is a fact about this
application.
"""

import re

from dataclasses import dataclass
from urllib.parse import urlencode
from collections.abc import Callable, Awaitable

from starlette.routing import Route
from starlette.requests import Request
from starlette.responses import Response

BASE = "/campaigns/native"

PATH_PARAM = re.compile(r"\{(?P<name>\w+)(?::\w+)?\}")


@dataclass(frozen=True)
class View:
    """The reader's filters and update transport, parsed once per request."""

    q: str = ""
    state: str = ""
    page: int = 1
    transport: str = "sse"

    @property
    def trigger(self) -> str:
        # The slow poll also recovers if an SSE connection is unavailable.
        return (
            "campaign-changed from:body, every 15s"
            if self.transport == "sse"
            else "every 3s"
        )


@dataclass(frozen=True)
class Resource:
    """A path template plus the View fields every URL to it must carry."""

    path: str
    carries: tuple[str, ...] = ()

    def url(self, view: View | None = None, /, **params: object) -> str:
        """Fill path parameters from ``params``; the rest become the query.

        Fields named in ``carries`` are copied from ``view`` first, so an
        explicit parameter of the same name overrides them.
        """
        filled = PATH_PARAM.sub(
            lambda match: str(params.pop(match["name"])), self.path
        )
        query = {name: getattr(view, name) for name in self.carries if view}
        query.update(params)
        return f"{filled}?{urlencode(query)}" if query else filled

    def route(
        self, endpoint: Callable[[Request], Awaitable[Response]]
    ) -> Route:
        return Route(self.path, endpoint)


DASHBOARD = Resource(BASE, ("q", "state", "page", "transport"))
SUMMARY = Resource(f"{BASE}/summary", ("transport",))
UPDATES = Resource(f"{BASE}/updates", ("q", "state", "page", "transport"))
EVENTS = Resource(f"{BASE}/events")
BUILD = Resource("/builds/{build_id:int}", ("transport",))
BUILD_STATUS = Resource("/builds/{build_id:int}/status", ("transport",))
LOG = Resource("/builds/{build_id:int}/log", ("transport",))
