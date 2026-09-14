"""Contracts of tagflow.htmx and tagflow.responses."""

import pytest
from bs4 import BeautifulSoup
from starlette.requests import Request

from tagflow import tag, text, document
from tagflow import htmx as hx
from tagflow.responses import matches, render_response


def request(method: str = "GET", **headers: str) -> Request:
    return Request(
        {
            "type": "http",
            "method": method,
            "path": "/",
            "query_string": b"",
            "headers": [
                (k.lower().encode(), v.encode()) for k, v in headers.items()
            ],
        }
    )


def hello() -> None:
    with tag.p():
        text("hello")


def attributes(component) -> dict[str, str]:
    with document() as doc:
        component()
    tags = BeautifulSoup(doc.to_html(), "html.parser").find_all()
    assert len(tags) == 1
    return {k: str(v) for k, v in tags[0].attrs.items()}


@pytest.mark.parametrize(
    "header, etag, expected",
    [
        ('"a"', '"a"', True),
        ('W/"a"', '"a"', True),  # weak comparison ignores weakness
        ('"b", "a"', '"a"', True),  # any listed tag matches
        ('"b" , W/"a"', '"a"', True),
        ("*", '"a"', True),
        ('"a"', '"ab"', False),  # exact opaque tag, not a prefix
        ('"a,b"', '"b"', False),  # commas inside a tag do not split it
        ("", '"a"', False),
        ('"b"', '"a"', False),
    ],
)
def test_if_none_match_comparison(header, etag, expected):
    assert matches(header, etag) is expected


def test_conditional_get_and_head_return_304_with_validators():
    first = render_response(request(), hello, cache_control="public")
    assert first.status_code == 200
    assert first.body == b"<p>hello</p>"
    etag = first.headers["etag"]
    for method in ("GET", "HEAD"):
        again = render_response(
            request(method, **{"If-None-Match": f"W/{etag}"}),
            hello,
            cache_control="public",
        )
        assert again.status_code == 304
        assert again.headers["etag"] == etag
        assert again.headers["cache-control"] == "public"


def test_conditional_semantics_only_apply_to_safe_methods():
    etag = render_response(
        request(), hello, cache_control="public"
    ).headers["etag"]
    posted = render_response(
        request("POST", **{"If-None-Match": etag}),
        hello,
        cache_control="public",
    )
    assert posted.status_code == 200


def test_unconditional_response_has_no_validator_and_keeps_headers():
    response = render_response(
        request(**{"If-None-Match": "*"}),
        hello,
        cache_control="no-store",
        doctype=True,
        conditional=False,
        headers={"HX-Retarget": "closest #x"},
        status_code=409,
    )
    assert response.status_code == 409
    assert bytes(response.body).startswith(b"<!doctype html>\n<p>")
    assert "etag" not in response.headers
    assert response.headers["cache-control"] == "no-store"
    assert response.headers["hx-retarget"] == "closest #x"
    assert response.media_type == "text/html"


def test_etag_changes_with_the_representation():
    def other() -> None:
        with tag.p():
            text("hello!")

    a = render_response(request(), hello, cache_control="public")
    b = render_response(request(), other, cache_control="public")
    assert a.headers["etag"] != b.headers["etag"]


def test_navigation_updates_history_but_preview_does_not():
    def nav() -> None:
        with tag.a(href="/x"):
            hx.navigate("/x", region="#workspace", indicator="#loading")

    def pre() -> None:
        with tag.a(href="/x"):
            hx.preview("/x", region="#panel")

    navigation, preview = attributes(nav), attributes(pre)
    assert navigation["hx-push-url"] == "true"
    assert navigation["hx-swap"] == "outerHTML"
    assert (
        navigation["hx-select"] == navigation["hx-target"] == "#workspace"
    )
    assert navigation["hx-sync"] == "#workspace:replace"
    assert navigation["hx-indicator"] == "#loading"
    assert "hx-push-url" not in preview
    assert "ignoreTitle:true" in preview["hx-swap"]
    assert preview["hx-select"] == preview["hx-target"] == "#panel"
    assert preview["hx-sync"] == "#panel:replace"


def test_refresh_morphs_in_place_and_stops_when_done():
    def live() -> None:
        with tag.div(id="s"):
            hx.refresh("/s", trigger="changed from:body, every 15s")

    def finished() -> None:
        with tag.div(id="s"):
            hx.refresh("/s", trigger="every 3s", done=True)

    active, done = attributes(live), attributes(finished)
    assert active["hx-trigger"] == "changed from:body, every 15s"
    assert active["hx-swap"] == "outerMorph"
    assert active["hx-sync"] == "this:replace"
    assert done["hx-trigger"] == "none"
    assert done["hx-get"] == "/s"


def test_cursor_reader_retries_on_interval_and_click_without_cancelling():
    def following() -> None:
        with tag.a(href="/next"):
            hx.read_cursor("/next", select="#chunk > *", every="2s")

    def paused() -> None:
        with tag.a(href="/next"):
            hx.read_cursor("/next", select="#chunk > *")

    live, held = attributes(following), attributes(paused)
    assert live["hx-trigger"] == "every 2s, click"
    assert live["hx-sync"] == "this:drop"
    assert live["hx-select"] == "#chunk > *"
    assert "ignoreTitle:true" in live["hx-swap"]
    assert held["hx-trigger"] == "click"
    assert held["hx-sync"] == "this:drop"


def test_connect_names_the_stream_and_its_closing_event():
    def source() -> None:
        with tag.div():
            hx.connect("/events", close_on="done")

    assert attributes(source) == {
        "hx-sse:connect": "/events",
        "hx-sse:close": "done",
    }


def test_recovery_targets_the_requesting_reader_only():
    headers = hx.recover_reader(closest="#build-detail")
    assert headers["HX-Retarget"] == "closest #build-detail"
    assert headers["HX-Reselect"] == "#build-detail"
    assert "ignoreTitle:true" in headers["HX-Reswap"]
    with pytest.raises(TypeError):
        hx.recover_reader("#build-detail")  # type: ignore[misc]
