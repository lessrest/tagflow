"""Run: uv run --all-extras hypercorn examples.dashboard.app:app"""

from collections.abc import AsyncGenerator, Callable
from contextlib import asynccontextmanager
from pathlib import Path

import anyio
from starlette.applications import Starlette
from starlette.exceptions import HTTPException
from starlette.requests import Request
from starlette.responses import (
    RedirectResponse,
    Response,
    StreamingResponse,
)
from starlette.routing import Mount, Route
from starlette.staticfiles import StaticFiles

from tagflow import tag, text

from . import hx, views
from .model import Build, Campaign, Snapshot, STATES
from .views import BASE, View
from .responses import render_response


def integer(value: str, minimum: int = 0, maximum: int = 10000) -> int:
    try:
        number = int(value)
    except ValueError:
        raise HTTPException(400, "Expected an integer") from None
    if not minimum <= number <= maximum:
        raise HTTPException(400, "Number outside the supported range")
    return number


def options(request: Request) -> View:
    params = request.query_params
    state = params.get("state", "")
    transport = params.get("transport", "sse")
    q = params.get("q", "").strip()
    if (
        state not in ("", *STATES)
        or transport not in ("sse", "poll")
        or len(q) > 80
    ):
        raise HTTPException(400, "Invalid dashboard filters")
    return View(q, state, integer(params.get("page", "1"), 1), transport)


def representation(
    request: Request,
    component: Callable[[], None],
    *,
    page: bool = False,
) -> Response:
    # This demo's data is public: representations may be stored but must be
    # revalidated. Validators hash the actual bytes, not a loose event cursor.
    return render_response(
        request, component, cache_control="public, no-cache", doctype=page
    )


def snapshot(request: Request) -> Snapshot:
    return request.app.state.campaign.snapshot


def build_for(request: Request, current: Snapshot) -> Build:
    ident = request.path_params["build_id"]
    for build in current.builds:
        if build.id == ident:
            return build
    raise HTTPException(404, "Build not found")


async def home(request: Request) -> Response:
    return RedirectResponse(BASE)


async def dashboard(request: Request) -> Response:
    current, view = snapshot(request), options(request)
    return representation(
        request,
        lambda: views.shell(
            "Native build campaign",
            lambda: views.dashboard(current, view),
        ),
        page=True,
    )


async def summary(request: Request) -> Response:
    current, view = snapshot(request), options(request)
    return representation(request, lambda: views.summary(current, view))


async def updates(request: Request) -> Response:
    current, view = snapshot(request), options(request)
    seen = request.query_params.get("seen", "")
    if len(seen) > 80:
        raise HTTPException(400, "Invalid revision")
    return representation(
        request, lambda: views.updates(current, view, seen)
    )


def build_page_content(
    request: Request, current: Snapshot, view: View
) -> tuple[str, Callable[[], None]]:
    build = build_for(request, current)
    follow = bool(integer(request.query_params.get("follow", "1"), 0, 1))

    def content() -> None:
        with tag.main(
            ["mx-auto", "max-w-2xl", "px-4", "py-3"], id="workspace"
        ):
            views.connection(current, view)
            with tag.a([views.LINK, "mb-2", "inline-block"], href=BASE):
                text("← Campaign overview")
            views.detail(build, current, view, follow)

    return build.name, content


async def build_page(request: Request) -> Response:
    current, view = snapshot(request), options(request)
    title, content = build_page_content(request, current, view)
    return representation(
        request, lambda: views.shell(title, content), page=True
    )


async def build_status(request: Request) -> Response:
    current, view = snapshot(request), options(request)
    build = build_for(request, current)
    return representation(request, lambda: views.build_status(build, view))


async def log(request: Request) -> Response:
    current, view = snapshot(request), options(request)
    build = build_for(request, current)
    after = integer(request.query_params.get("after", "0"))
    follow = bool(integer(request.query_params.get("follow", "1"), 0, 1))
    epoch = request.query_params.get("epoch", current.epoch)
    if epoch != current.epoch or after > len(build.lines):
        # A restarted simulation is a different log, never silently reuse an
        # old offset. Replace the reader that asked, not append duplicate lines
        # and not whichever newer reader now carries the same ID.
        title, content = build_page_content(request, current, view)
        return render_response(
            request,
            lambda: views.shell(title, content),
            cache_control="no-store",
            doctype=True,
            conditional=False,
            headers=hx.recover_reader(closest="#build-detail"),
        )
    return representation(
        request,
        lambda: views.shell(
            f"{build.name}: build output",
            lambda: views.log_page(build, current, after, follow, view),
        ),
        page=True,
    )


async def notifications(campaign: Campaign) -> AsyncGenerator[str, None]:
    # Notifications are lossy/coalescible hints, not an event ledger. Always
    # announce the current revision on connect (including reconnect) so a gap,
    # unknown Last-Event-ID, or process restart repairs through ordinary GETs.
    previous = ""
    idle = 0
    while True:
        current = campaign.snapshot
        if current.revision != previous:
            yield f"id: {current.revision}\nevent: campaign-changed\ndata: {current.revision}\n\n"
            previous = current.revision
            idle = 0
        if current.complete:
            yield "event: campaign-complete\ndata: complete\n\n"
            return
        await anyio.sleep(1)
        idle += 1
        if idle % 10 == 0:
            yield ": heartbeat\n\n"


async def events(request: Request) -> Response:
    return StreamingResponse(
        notifications(request.app.state.campaign),
        media_type="text/event-stream",
        headers={"Cache-Control": "no-store", "X-Accel-Buffering": "no"},
    )


def create_app(
    campaign: Campaign | None = None, *, simulate: bool = True
) -> Starlette:
    shared = campaign or Campaign()

    @asynccontextmanager
    async def lifespan(app: Starlette):
        async with anyio.create_task_group() as tasks:
            if simulate:
                tasks.start_soon(shared.run)
            try:
                yield
            finally:
                tasks.cancel_scope.cancel()

    app = Starlette(
        lifespan=lifespan,
        routes=[
            Route("/", home),
            Route(BASE, dashboard),
            Route(f"{BASE}/summary", summary),
            Route(f"{BASE}/updates", updates),
            Route(f"{BASE}/events", events, methods=["GET"]),
            Route("/builds/{build_id:int}", build_page),
            Route("/builds/{build_id:int}/status", build_status),
            Route("/builds/{build_id:int}/log", log),
            Mount(
                "/static",
                StaticFiles(directory=Path(__file__).parent / "static"),
            ),
        ],
    )
    app.state.campaign = shared
    return app


app = create_app()
