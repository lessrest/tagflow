import time

import anyio
import pytest
from starlette.requests import Request
from starlette.responses import PlainTextResponse
from starlette.testclient import TestClient
from starlette.websockets import WebSocketDisconnect
from starlette.applications import Starlette
from starlette.routing import Route

from tagflow import (
    Live,
    Region,
    Session,
    document,
    render_region,
    tag,
    text,
)


def counter(value: int) -> None:
    with tag.output(id="counter"):
        text(str(value))


def status(value: str) -> None:
    with tag.p(id="status"):
        text(value)


def test_render_region_requires_one_identified_root():
    assert render_region(lambda: counter(3)) == Region(
        "counter", '<output id="counter">3</output>'
    )

    def two() -> None:
        counter(1)
        counter(2)

    def anonymous() -> None:
        with tag.div():
            text("x")

    with pytest.raises(ValueError, match="exactly one root"):
        render_region(two)
    with pytest.raises(ValueError, match="exactly one root"):
        render_region(lambda: None)
    with pytest.raises(ValueError, match="<div> must have an id"):
        render_region(anonymous)


def live_app(grace: float = 30.0) -> tuple[Starlette, Live]:
    """A page with two regions and endpoints that drive updates."""
    live = Live(grace=grace)
    sessions: dict[str, Session] = {}
    pages: dict[str, str] = {}

    async def page(request: Request) -> PlainTextResponse:
        session = await live.session()
        sessions[request.query_params["name"]] = session
        with document() as doc:
            with tag.body():
                session.client_tag()
                session.mount(lambda: counter(0))
                text("trailing text belongs to the body")
        session.update(lambda: status("ready"))
        pages[session.id] = doc.to_html()
        return PlainTextResponse(session.id)

    async def bump(request: Request) -> PlainTextResponse:
        session = sessions[request.query_params["name"]]
        value = int(request.query_params["value"])
        session.update(lambda: counter(value))
        return PlainTextResponse("ok")

    async def cancel(request: Request) -> PlainTextResponse:
        sessions[request.query_params["name"]].cancel()
        return PlainTextResponse("ok")

    app = Starlette(
        lifespan=live.run,
        routes=[
            Route("/page", page),
            Route("/bump", bump),
            Route("/cancel", cancel),
        ],
    )
    app.state.pages = pages
    return app, live


def morphs(message: dict) -> dict[str, str]:
    assert message["type"] == "update"
    return {m["target"]: m["html"] for m in message["morphs"]}


def test_unknown_session_is_closed_as_expired():
    app, _ = live_app()
    with TestClient(app) as client:
        with client.websocket_connect(Live.SOCKET) as ws:
            ws.send_json({"id": "nope"})
            with pytest.raises(WebSocketDisconnect) as closed:
                ws.receive_json()
            assert closed.value.code == 4001


def test_mount_renders_into_the_page_and_records_the_region():
    app, live = live_app()
    with TestClient(app) as client:
        session_id = client.get("/page", params={"name": "a"}).text
        assert app.state.pages[session_id] == (
            f'<body><tagflow-client session-id="{session_id}">'
            '</tagflow-client><output id="counter">0</output>'
            "trailing text belongs to the body</body>"
        )
        # Recorded without the body's trailing text, ready for a late
        # connection even though update() never touched it.
        assert live._sessions[session_id].regions["counter"] == (
            '<output id="counter">0</output>'
        )
        with client.websocket_connect(Live.SOCKET) as ws:
            ws.send_json({"id": session_id})
            assert morphs(ws.receive_json()) == {
                "counter": '<output id="counter">0</output>',
                "status": '<p id="status">ready</p>',
            }


def test_connection_receives_current_regions_then_changes():
    app, _ = live_app()
    with TestClient(app) as client:
        session_id = client.get("/page", params={"name": "a"}).text
        # Changes before any browser attaches are not lost, only coalesced.
        client.get("/bump", params={"name": "a", "value": 1})
        client.get("/bump", params={"name": "a", "value": 2})
        with client.websocket_connect(Live.SOCKET) as ws:
            ws.send_json({"id": session_id})
            assert morphs(ws.receive_json()) == {
                "counter": '<output id="counter">2</output>',
                "status": '<p id="status">ready</p>',
            }
            # A change to one region sends only that region.
            client.get("/bump", params={"name": "a", "value": 3})
            assert morphs(ws.receive_json()) == {
                "counter": '<output id="counter">3</output>'
            }
            # Re-rendering identical HTML sends nothing; the next real
            # change arrives alone.
            client.get("/bump", params={"name": "a", "value": 3})
            client.get("/bump", params={"name": "a", "value": 4})
            assert morphs(ws.receive_json()) == {
                "counter": '<output id="counter">4</output>'
            }
            # A second browser on the same session converges from scratch.
            with client.websocket_connect(Live.SOCKET) as other:
                other.send_json({"id": session_id})
                assert morphs(other.receive_json()) == {
                    "counter": '<output id="counter">4</output>',
                    "status": '<p id="status">ready</p>',
                }


def test_cancelled_session_closes_its_browsers_as_expired():
    app, live = live_app()
    with TestClient(app) as client:
        session_id = client.get("/page", params={"name": "a"}).text
        with client.websocket_connect(Live.SOCKET) as ws:
            ws.send_json({"id": session_id})
            ws.receive_json()
            client.get("/cancel", params={"name": "a"})
            with pytest.raises(WebSocketDisconnect) as closed:
                ws.receive_json()
            assert closed.value.code == 4001
        assert session_id not in live._sessions


def test_session_ends_only_after_grace_without_a_browser():
    app, live = live_app(grace=0.1)
    with TestClient(app) as client:
        session_id = client.get("/page", params={"name": "a"}).text
        with client.websocket_connect(Live.SOCKET) as ws:
            ws.send_json({"id": session_id})
            ws.receive_json()
            time.sleep(0.35)
            client.get("/bump", params={"name": "a", "value": 1})
            # Attached: still alive well past the grace period.
            assert morphs(ws.receive_json()) == {
                "counter": '<output id="counter">1</output>'
            }
        deadline = time.monotonic() + 2
        while session_id in live._sessions and time.monotonic() < deadline:
            time.sleep(0.05)
        assert session_id not in live._sessions


@pytest.mark.anyio
async def test_update_renders_everything_before_publishing_anything():
    async with anyio.create_task_group() as tasks:
        session = Session("s", tasks, grace=30.0)
        session.update(lambda: counter(1))

        def broken() -> None:
            with tag.div():
                text("no id")

        with pytest.raises(ValueError):
            session.update(lambda: counter(2), broken)
        assert session.regions == {
            "counter": '<output id="counter">1</output>'
        }
