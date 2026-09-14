"""Run: uv run --all-extras hypercorn examples.dashboard.app:app"""

from collections.abc import AsyncGenerator, Callable
from contextlib import asynccontextmanager
from hashlib import sha256
from pathlib import Path

import anyio
from starlette.applications import Starlette
from starlette.exceptions import HTTPException
from starlette.requests import Request
from starlette.responses import (
    HTMLResponse,
    RedirectResponse,
    Response,
    StreamingResponse,
)
from starlette.routing import Mount, Route
from starlette.staticfiles import StaticFiles

from tagflow import tag, text

from . import views
from .model import Build, Campaign, Snapshot, STATES
from .views import BASE, View


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
    conditional: bool = True,
) -> Response:
    body = ("<!doctype html>\n" if page else "") + views.render(component)
    # Hash the actual representation, not a loose event cursor. This tiny demo
    # trades render cost for correctness, including filters and template edits.
    etag = '"' + sha256(body.encode()).hexdigest() + '"'
    headers = {"ETag": etag, "Cache-Control": "public, no-cache"}
    candidates = request.headers.get("if-none-match", "").split(",")
    if conditional and any(
        value.strip().removeprefix("W/") in (etag, "*")
        for value in candidates
    ):
        return Response(status_code=304, headers=headers)
    return HTMLResponse(body, headers=headers)


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


async def build_page(
    request: Request, *, conditional: bool = True
) -> Response:
    current, view = snapshot(request), options(request)
    build = build_for(request, current)
    follow = bool(integer(request.query_params.get("follow", "1"), 0, 1))

    def content() -> None:
        with tag.main(
            ["mx-auto", "max-w-2xl", "px-4", "py-3"], id="workspace"
        ):
            with tag.a([views.LINK, "mb-2", "inline-block"], href=BASE):
                text("← Campaign overview")
            views.detail(build, current, view, follow)

    return representation(
        request,
        lambda: views.shell(build.name, content),
        page=True,
        conditional=conditional,
    )


async def build_status(request: Request) -> Response:
    current, view = snapshot(request), options(request)
    build = build_for(request, current)
    return representation(request, lambda: views.build_status(build, view))


async def log(request: Request) -> Response:
    current = snapshot(request)
    build = build_for(request, current)
    after = integer(request.query_params.get("after", "0"))
    follow = bool(integer(request.query_params.get("follow", "1"), 0, 1))
    epoch = request.query_params.get("epoch", current.epoch)
    if epoch != current.epoch or after > len(build.lines):
        # A restarted simulation is a different log, never silently reuse an
        # old offset. Let htmx replace the old reader, not append duplicate lines.
        response = await build_page(request, conditional=False)
        response.headers.update(
            {
                "HX-Retarget": "#build-detail",
                "HX-Reselect": "#build-detail",
                "HX-Reswap": "outerHTML",
            }
        )
        response.headers["Cache-Control"] = "no-store"
        return response
    return representation(
        request,
        lambda: views.log_window(build, current.epoch, after, follow),
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
