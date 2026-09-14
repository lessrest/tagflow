from contextlib import aclosing
from urllib.parse import urlsplit

import anyio
import httpx
import pytest
from bs4 import BeautifulSoup

from examples.dashboard.app import create_app, notifications
from examples.dashboard.model import Campaign
from examples.dashboard.views import BASE


@pytest.fixture
def campaign():
    return Campaign()


@pytest.fixture
async def client(campaign):
    async with httpx.AsyncClient(
        transport=httpx.ASGITransport(
            app=create_app(campaign, simulate=False)
        ),
        base_url="http://dashboard.test",
    ) as client:
        yield client


@pytest.mark.anyio
async def test_pages_are_html_with_or_without_htmx(client):
    page = await client.get(BASE)
    enhanced = await client.get(BASE, headers={"HX-Request": "true"})
    assert page.status_code == 200
    assert page.text == enhanced.text
    assert page.text.startswith("<!doctype html>")
    assert "set-cookie" not in page.headers
    soup = BeautifulSoup(page.text, "html.parser")
    assert soup.h1 is not None
    assert soup.h1.text == "Native build campaign"
    assert "simulated data" in soup.text
    assert "A different kind of live" not in soup.text
    assert [n.text for n in soup.select("#summary strong")] == [
        "18",
        "2",
        "4",
        "0",
    ]
    assert len(soup.select("#builds tbody tr")) == 8
    assert soup.select_one("[hx-sse\\:connect]") is not None
    assert soup.select_one('script[src*="htmx-4.0.0"]')
    for link in soup.select(".package-link"):
        response = await client.get(link["href"])
        assert response.status_code == 200
        assert "<!doctype html>" in response.text
    assert (await client.head(BASE)).content == b""


@pytest.mark.anyio
async def test_filters_pagination_and_empty_state(client):
    page = await client.get(
        BASE, params={"q": "sqlite", "transport": "poll"}
    )
    soup = BeautifulSoup(page.text, "html.parser")
    assert [a.text for a in soup.select(".package-link")] == ["sqlite"]
    assert not soup.select("[hx-sse\\:connect]")
    summary = soup.select_one("#summary")
    assert summary is not None
    assert summary["hx-trigger"] == "every 3s"
    empty = await client.get(BASE, params={"q": "no-such-build"})
    assert "No builds match" in empty.text
    second = await client.get(BASE, params={"page": "2"})
    assert "9–16 of 18 builds" in second.text
    escaped = await client.get(
        BASE, params={"q": '<script>alert("x")</script>'}
    )
    assert '<script>alert("x")</script>' not in escaped.text
    escaped_soup = BeautifulSoup(escaped.text, "html.parser")
    search = escaped_soup.select_one('input[name="q"]')
    assert search is not None
    assert search["value"] == '<script>alert("x")</script>'
    assert not escaped_soup.select("script:not([src])")


@pytest.mark.anyio
async def test_etags_validate_actual_representations(client, campaign):
    url = f"{BASE}/summary"
    initial = await client.get(url)
    etag = initial.headers["etag"]
    assert initial.headers["cache-control"] == "public, no-cache"
    for condition in (etag, f'"unrelated", W/{etag}', "*"):
        response = await client.get(
            url, headers={"If-None-Match": condition}
        )
        assert response.status_code == 304
        assert not response.content
        assert response.headers["etag"] == etag
    campaign.advance()
    changed = await client.get(url, headers={"If-None-Match": etag})
    assert changed.status_code == 200
    assert changed.headers["etag"] != etag
    # A finished build's status doesn't change when other builds advance.
    finished = await client.get("/builds/1/status")
    campaign.advance()
    stable = await client.get("/builds/1/status")
    assert finished.headers["etag"] == stable.headers["etag"]


@pytest.mark.anyio
async def test_inventory_notification_does_not_replace_rows(
    client, campaign
):
    seen = campaign.snapshot.revision
    params = {"seen": seen, "q": "lib", "state": "passed", "page": "2"}
    current = await client.get(f"{BASE}/updates", params=params)
    assert "List is up to date" in current.text
    campaign.advance()
    changed = await client.get(f"{BASE}/updates", params=params)
    soup = BeautifulSoup(changed.text, "html.parser")
    link = soup.select_one("a.update")
    assert link is not None
    assert "q=lib" in link["href"] and "page=2" in link["href"]
    assert not soup.select("table")
    updates = soup.select_one("#updates")
    assert updates is not None
    assert seen in updates["hx-get"]


@pytest.mark.anyio
async def test_log_pages_are_bounded_and_repeatable(client, campaign):
    url = "/builds/1/log"
    first = await client.get(url)
    soup = BeautifulSoup(first.text, "html.parser")
    first_ids = [line["id"] for line in soup.select(".log-line")]
    assert len(first_ids) == 5
    tail = soup.select_one(".log-tail")
    assert tail is not None
    cursor = tail["href"]
    second = await client.get(cursor)
    repeated = await client.get(cursor)
    assert second.text == repeated.text
    second_ids = [
        line["id"]
        for line in BeautifulSoup(second.text, "html.parser").select(
            ".log-line"
        )
    ]
    assert not set(first_ids) & set(second_ids)
    assert len(first_ids + second_ids) == len(
        campaign.snapshot.builds[0].lines
    )
    assert "End of build log" in second.text
    assert "hx-trigger" not in second.text


@pytest.mark.anyio
async def test_log_pause_and_reset(client):
    paused = await client.get("/builds/6?follow=0")
    assert "Resume following" in paused.text
    assert "load delay:2s" not in paused.text
    reset = await client.get(
        "/builds/6/log?epoch=old-process&after=100",
        headers={"If-None-Match": "*"},
    )
    assert reset.status_code == 200
    assert reset.headers["cache-control"] == "no-store"
    assert reset.headers["hx-retarget"] == "#build-detail"
    assert reset.headers["hx-reswap"] == "outerHTML"
    assert 'id="build-detail"' in reset.text


@pytest.mark.anyio
@pytest.mark.parametrize(
    "path",
    [
        BASE + "?page=bad",
        BASE + "?page=0",
        BASE + "?state=unknown",
        BASE + "?transport=websocket",
        "/builds/1/log?after=-1",
        "/builds/1?follow=4",
    ],
)
async def test_bad_queries_are_rejected(client, path):
    assert (await client.get(path)).status_code == 400


@pytest.mark.anyio
async def test_missing_build_and_no_mutations(client):
    assert (await client.get("/builds/999")).status_code == 404
    assert (await client.post(BASE)).status_code == 405
    page = BeautifulSoup((await client.get(BASE)).text, "html.parser")
    for script in page.select("script[src]"):
        src = script["src"]
        assert isinstance(src, str)
        assert not urlsplit(src).netloc
        assert (await client.get(src)).status_code == 200


@pytest.mark.anyio
async def test_sse_connect_reconnect_coalescing_and_completion(campaign):
    async with aclosing(notifications(campaign)) as stream:
        first = await anext(stream)
        assert f"id: {campaign.snapshot.revision}" in first
        assert "event: campaign-changed" in first
        campaign.advance()
        campaign.advance()
        with anyio.fail_after(2):
            latest = await anext(stream)
        assert f"data: {campaign.snapshot.revision}" in latest
    # A reconnect always announces current state, without replaying mutations.
    async with aclosing(notifications(campaign)) as reconnected:
        assert await anext(reconnected) == latest
    campaign.snapshot = campaign.at(40)
    events = [event async for event in notifications(campaign)]
    assert len(events) == 2
    assert "campaign-complete" in events[-1]


@pytest.mark.anyio
async def test_stream_http_headers_and_no_replay_dependency(campaign):
    campaign.snapshot = campaign.at(40)
    async with httpx.AsyncClient(
        transport=httpx.ASGITransport(
            app=create_app(campaign, simulate=False)
        ),
        base_url="http://dashboard.test",
    ) as client:
        response = await client.get(
            BASE + "/events", headers={"Last-Event-ID": "old-epoch.999"}
        )
        assert response.headers["content-type"].startswith(
            "text/event-stream"
        )
        assert response.headers["cache-control"] == "no-store"
        assert response.headers["x-accel-buffering"] == "no"
        assert campaign.snapshot.revision in response.text


@pytest.mark.anyio
async def test_lifespan_shutdown_is_bounded():
    app = create_app()
    with anyio.fail_after(1):
        async with app.router.lifespan_context(app):
            await anyio.sleep(0)
