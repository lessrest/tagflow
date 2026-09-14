"""htmx 4 attribute and header bundles, each naming one reading contract.

Every function below sets attributes on the current Tagflow element, exactly
like calling ``attr()`` by hand. The bundles exist because the burden in an
HTML-first dashboard is not emitting attributes; it is remembering which
combination encodes which contract. Navigation changes history and the title;
a preview changes neither; a cursor reader must keep retrying without
cancelling a slow response; recovery must replace the reader that asked, not
whichever current reader happens to carry the same ID.

The generated attributes stay visible in the HTML and ``attr()`` remains the
escape hatch. Nothing here stores state on the server, chooses a transport,
or knows about the server's routes. The vocabulary follows htmx 4; load
``htmx.min.js`` (and ``hx-sse`` for :func:`connect`) yourself.

Usage::

    from tagflow import tag, text
    from tagflow import htmx as hx

    with tag.section(id="summary"):
        hx.refresh("/summary", trigger="every 3s", done=finished)
        ...
"""

from .tagflow import attr


def navigate(
    url: str, *, region: str, indicator: str | None = None
) -> None:
    """Enhance a real link or GET form into a same-page navigation.

    The server returns the same full page it would return without htmx;
    ``hx-select`` extracts ``region`` and replaces the local one. The browser
    URL and title follow the response. Concurrent navigations replace each
    other so the newest request wins.
    """
    attr("hx-get", url)
    attr("hx-select", region)
    attr("hx-target", region)
    attr("hx-swap", "outerHTML")
    attr("hx-push-url", "true")
    attr("hx-sync", f"{region}:replace")
    if indicator:
        attr("hx-indicator", indicator)


def preview(url: str, *, region: str) -> None:
    """Load ``region`` from a full page into the same region here.

    Unlike :func:`navigate`, a preview changes neither history nor the tab
    title, so the title keeps matching the URL in the address bar. Use a real
    ``href`` on the element so the full page remains reachable without JS.
    """
    attr("hx-get", url)
    attr("hx-select", region)
    attr("hx-target", region)
    attr("hx-swap", "outerHTML ignoreTitle:true")
    attr("hx-sync", f"{region}:replace")


def refresh(url: str, *, trigger: str, done: bool = False) -> None:
    """Make the current element a self-refreshing embedded representation.

    The element fetches its own resource on ``trigger`` and morphs itself in
    place, preserving focus and unchanged nodes. Overlapping refreshes replace
    each other; the latest state wins. ``done`` disables the trigger for
    resources that will not change again, so completed pages stop polling.
    """
    attr("hx-get", url)
    attr("hx-trigger", "none" if done else trigger)
    attr("hx-swap", "outerMorph")
    attr("hx-sync", "this:replace")


def read_cursor(url: str, *, select: str, every: str | None = None) -> None:
    """Replace the current element with the next page of a cursor feed.

    The response is a full page; ``select`` picks the chunk whose children
    (new lines plus the next cursor control) replace this element. A recurring
    read fires ``every`` interval and also on click, so a failed request does
    not stop following and a reader can retry immediately. ``this:drop``
    ignores a tick while a slow response is in flight rather than cancelling
    it, which is what previously left readers stuck after one failure.
    """
    attr("hx-get", url)
    attr("hx-select", select)
    attr("hx-swap", "outerHTML ignoreTitle:true")
    attr("hx-trigger", f"every {every}, click" if every else "click")
    attr("hx-sync", "this:drop")


def connect(url: str, *, close_on: str | None = None) -> None:
    """Open an SSE connection whose named events other elements can trigger on.

    Place this element outside any region that :func:`preview` or
    :func:`navigate` may replace, or it disappears with the first swap. This
    helper cannot verify that placement; the browser regressions do.
    """
    attr("hx-sse:connect", url)
    if close_on:
        attr("hx-sse:close", close_on)


def recover_reader(*, closest: str) -> dict[str, str]:
    """Response headers that replace the reader which made the request.

    A late or reset response must not look up ``closest`` globally: a newer
    reader may now own that ID. ``closest`` is resolved relative to the
    requesting element, so a detached old reader cannot reacquire the panel
    that replaced it. There is deliberately no global-target variant.
    """
    return {
        "HX-Retarget": f"closest {closest}",
        "HX-Reselect": closest,
        "HX-Reswap": "outerHTML ignoreTitle:true",
    }
