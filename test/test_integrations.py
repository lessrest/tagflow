import anyio
import httpx
import pytest
from fastapi import FastAPI

from tagflow import (
    DocumentMiddleware,
    Live,
    TagResponse,
    tag,
    text,
)


@pytest.mark.anyio
async def test_fastapi_document_middleware_isolates_requests():
    app = FastAPI(default_response_class=TagResponse)
    app.add_middleware(DocumentMiddleware)

    @app.get("/sync/{name}")
    def sync_page(name: str):
        with tag.p():
            text(name)

    @app.get("/async/{name}")
    async def async_page(name: str):
        await anyio.sleep(0)
        with tag.p():
            text(name)

    async with httpx.AsyncClient(
        transport=httpx.ASGITransport(app=app), base_url="http://test"
    ) as client:
        for path, name in [
            ("sync", "one"),
            ("async", "two"),
            ("sync", "three"),
        ]:
            response = await client.get(f"/{path}/{name}")
            assert response.status_code == 200
            assert response.text == f"<!doctype html>\n<p>{name}</p>"
            assert (
                response.headers["content-type"]
                == "text/html; charset=utf-8"
            )


@pytest.mark.anyio
async def test_live_shutdown_cancels_sessions():
    live = Live()
    started = anyio.Event()
    stopped = anyio.Event()

    async def background():
        try:
            started.set()
            await anyio.sleep_forever()
        finally:
            stopped.set()

    with anyio.fail_after(2):
        async with live.run(FastAPI()):
            session = await live.session()
            session.spawn(background)
            await started.wait()
        assert stopped.is_set()
        assert not live._sessions
        with pytest.raises(RuntimeError, match="Live.run"):
            await live.session()
