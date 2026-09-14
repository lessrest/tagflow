"""HTML representations. All application UI is constructed by Tagflow."""

from collections.abc import Callable
from dataclasses import dataclass, replace
from urllib.parse import urlencode

from tagflow import tag, text, ClassValue

from . import hx
from .model import Build, Snapshot, STATES


BASE = "/campaigns/native"
PAGE_SIZE = 8

# Complete utility tokens let Tailwind discover every branch at build time.
# Tagflow flattens these nested lists; no joins or CSS component layer needed.
FRAME: ClassValue = [
    "mx-auto",
    "max-w-360",
    ["px-4", "md:px-6", "xl:px-10"],
]
PANEL: ClassValue = [
    "rounded-lg",
    "border",
    "border-slate-200",
    "bg-white",
    "overflow-hidden",
]
MUTED: ClassValue = ["text-slate-500"]
SMALL: ClassValue = [MUTED, "text-xs"]
TABLE_HEADING: ClassValue = [
    MUTED,
    "text-xs",
    "font-semibold",
]
FOCUS: ClassValue = [
    "focus-visible:outline-2",
    "focus-visible:outline-offset-4",
    "focus-visible:outline-blue-500",
]
LINK: ClassValue = [FOCUS, "text-blue-700", "hover:underline"]
BUTTON: ClassValue = [
    FOCUS,
    ["inline-flex", "items-center", "justify-center"],
    ["rounded-md", "border", "border-slate-200"],
    ["px-2", "py-1", "text-xs", "cursor-pointer", "hover:bg-slate-100"],
]
FIELD: ClassValue = [
    FOCUS,
    ["rounded-md", "border", "border-slate-200", "bg-white"],
    ["px-2", "py-1", "text-xs", "text-slate-700"],
]
HEADING: ClassValue = ["text-lg", "font-semibold", "tracking-tight"]
PANEL_TITLE: ClassValue = [
    "flex",
    "flex-wrap",
    "items-center",
    "justify-between",
    "gap-3",
]
BADGES: dict[str, ClassValue] = {
    "passed": ["bg-emerald-50", "text-emerald-800"],
    "running": ["bg-blue-50", "text-blue-700"],
    "failed": ["bg-red-50", "text-red-800"],
    "queued": ["bg-slate-100", "text-slate-600"],
}
NUMBERS = {
    "passed": "text-emerald-700",
    "running": "text-blue-700",
    "failed": "text-red-700",
}


@dataclass(frozen=True)
class View:
    q: str = ""
    state: str = ""
    page: int = 1
    transport: str = "sse"

    def query(self) -> str:
        return urlencode(
            dict(
                q=self.q,
                state=self.state,
                page=self.page,
                transport=self.transport,
            )
        )

    def url(self) -> str:
        return f"{BASE}?{self.query()}"

    @property
    def trigger(self) -> str:
        # The slow poll also recovers if an SSE connection is unavailable.
        return (
            "campaign-changed from:body, every 15s"
            if self.transport == "sse"
            else "every 3s"
        )


def label(value: str, kind: str = "") -> None:
    with tag.span(
        [
            "badge",
            "inline-block",
            "rounded",
            "px-2",
            "py-0.5",
            "text-[10px]",
            "font-medium",
        ],
        BADGES.get(kind, ["bg-slate-100", "text-slate-600"]),
    ):
        text(value)


def navigation(url: str) -> None:
    """Enhance a real link or GET form, without negotiating fragments."""
    hx.navigate(url, region="#workspace", indicator="#loading")


def shell(title: str, content: Callable[[], None]) -> None:
    with tag.html(lang="en"):
        with tag.head():
            with tag.title():
                text(f"{title} · Tagflow")
            with tag.meta(charset="utf-8"):
                pass
            with tag.meta(
                name="viewport",
                content="width=device-width, initial-scale=1",
            ):
                pass
            with tag.meta(
                name="htmx-config",
                content='{"noSwap":[204,304,"4xx","5xx"]}',
            ):
                pass
            with tag.link(rel="stylesheet", href="/static/dashboard.css"):
                pass
            for script in ("htmx-4.0.0.min.js", "hx-sse-4.0.0.min.js"):
                with tag.script(src=f"/static/{script}", defer=True):
                    pass
        with tag.body(
            [
                "bg-slate-50",
                "text-slate-800",
                "font-[system-ui,sans-serif]",
                "text-sm",
                "antialiased",
            ]
        ):
            with tag.a(
                [
                    LINK,
                    "sr-only",
                    "focus:not-sr-only",
                    "focus:block",
                    "focus:p-3",
                ],
                href="#workspace",
            ):
                text("Skip to campaign")
            with tag.header(
                [
                    FRAME,
                    "flex",
                    "h-10",
                    "items-center",
                    "gap-3",
                    "border-b",
                    "border-slate-200",
                ]
            ):
                with tag.a(
                    [FOCUS, "font-semibold"],
                    href=BASE,
                ):
                    text("Tagflow")
                with tag.span(SMALL):
                    text("Dashboard demo · simulated data")
            content()


def summary(snapshot: Snapshot, view: View) -> None:
    with tag.section(
        id="summary",
        aria_label="Live campaign summary",
        data_revision=snapshot.revision,
    ):
        hx.refresh(
            f"{BASE}/summary?transport={view.transport}",
            trigger=view.trigger,
            done=snapshot.complete,
        )
        counts = {
            s: sum(b.state == s for b in snapshot.builds) for s in STATES
        }
        with tag.div(
            [PANEL, "grid", "grid-cols-4", "divide-x", "divide-slate-200"]
        ):
            for name, value, kind in (
                ("Builds", len(snapshot.builds), ""),
                ("Passed", counts["passed"], "passed"),
                ("Running", counts["running"], "running"),
                ("Failed", counts["failed"], "failed"),
            ):
                with tag.div(["grid", "gap-0.5", "px-3", "py-2"]):
                    with tag.span(SMALL):
                        text(name)
                    with tag.strong(
                        [
                            "text-xl",
                            "font-medium",
                            "tracking-tight",
                            "tabular-nums",
                            NUMBERS.get(kind),
                        ]
                    ):
                        text(str(value))
        done = counts["passed"] + counts["failed"]
        with tag.div(
            [
                SMALL,
                "mt-1",
                "flex",
                "justify-between",
                "gap-4",
                "max-sm:text-[10px]",
            ]
        ):
            with tag.span():
                text(f"{done} of {len(snapshot.builds)} builds resolved")
            with tag.span():
                text(
                    "Campaign complete"
                    if snapshot.complete
                    else f"{counts['queued']} queued"
                )
        with tag.progress(
            [
                "mt-1",
                "block",
                "h-1",
                "w-full",
                "overflow-hidden",
                "rounded",
                "appearance-none",
                "bg-slate-200",
                "[&::-webkit-progress-bar]:bg-slate-200",
                "[&::-webkit-progress-value]:bg-blue-400",
                "[&::-moz-progress-bar]:bg-blue-400",
            ],
            value=done,
            max=len(snapshot.builds),
            aria_label="Resolved builds",
        ):
            text(f"{done} / {len(snapshot.builds)}")


def updates(snapshot: Snapshot, view: View, seen: str) -> None:
    url = f"{BASE}/updates?{view.query()}&{urlencode({'seen': seen})}"
    with tag.div(
        ["text-xs", "whitespace-nowrap"],
        id="updates",
        aria_live="polite",
    ):
        hx.refresh(url, trigger=view.trigger, done=snapshot.complete)
        if seen != snapshot.revision:
            with tag.a(
                [BUTTON, "update", "bg-blue-50", "text-blue-700"],
                href=view.url(),
            ):
                navigation(view.url())
                text("Updates available →")
        else:
            with tag.span(MUTED):
                text("List is up to date")


def build_status(build: Build, view: View) -> None:
    with tag.div(id="build-status"):
        hx.refresh(
            f"/builds/{build.id}/status?transport={view.transport}",
            trigger=view.trigger,
            done=build.state not in ("queued", "running"),
        )
        label(build.state.title(), build.state)
        with tag.p([SMALL, "mt-1"]):
            text(build.phase)


def log_window(
    build: Build, epoch: str, after: int, follow: bool, view: View
) -> None:
    end = min(after + 5, len(build.lines))
    for offset in range(after, end):
        with tag.div(
            ["log-line", "flex", "items-baseline", "gap-2.5"],
            id=f"log-{epoch}-{build.id}-{offset}",
        ):
            with tag.span(["text-slate-400", "select-none"]):
                text(str(offset + 1).zfill(2))
            with tag.code(
                [
                    "whitespace-pre-wrap",
                    "wrap-anywhere",
                    "font-mono",
                    "text-slate-600",
                ]
            ):
                text(build.lines[offset])
    if end < len(build.lines) or (
        follow and build.state in ("queued", "running")
    ):
        url = f"/builds/{build.id}/log?{urlencode(dict(epoch=epoch, after=end, follow=int(follow), transport=view.transport))}"
        with tag.a(
            [LINK, "log-tail", "mt-2", "block", "text-[10px]"],
            href=url,
        ):
            hx.read_cursor(
                url,
                select="#log-chunk > *",
                every="2s" if follow else None,
            )
            text(
                "Waiting for output…"
                if end == len(build.lines)
                else "Read next lines →"
            )
    else:
        with tag.p([MUTED, "mt-2", "text-[10px]"]):
            text(
                "End of build log."
                if build.state in ("passed", "failed")
                else "Following paused. Resume to read new output."
            )


def detail(
    build: Build, snapshot: Snapshot, view: View, follow: bool = True
) -> None:
    with tag.aside(
        [PANEL, "p-3"],
        id="build-detail",
        aria_label="Build details",
    ):
        with tag.div([PANEL_TITLE, "mb-2"]):
            with tag.h2(["text-base", "font-semibold"]):
                text(build.name)
            with tag.a([LINK, "text-xs"], href=f"/builds/{build.id}"):
                text("Permalink ↗")
        build_status(build, view)
        with tag.dl(
            [
                "my-2",
                "grid",
                "grid-cols-[80px_1fr]",
                "gap-1",
                "border-y",
                "border-slate-200",
                "py-2",
                "text-xs",
            ]
        ):
            for name, value in (
                ("Platform", "x86_64-linux"),
                ("Builder", f"worker-{(build.id - 1) % 4 + 1:02}"),
            ):
                with tag.dt(MUTED):
                    text(name)
                with tag.dd():
                    text(value)
        with tag.div([PANEL_TITLE, "mb-2"]):
            with tag.h3(["text-sm", "font-semibold"]):
                text("Build output")
            url = f"/builds/{build.id}?follow={int(not follow)}&transport={view.transport}"
            with tag.a([LINK, "text-[10px]"], href=url):
                hx.preview(url, region="#build-detail")
                text("Pause following" if follow else "Resume following")
        with tag.div(
            [
                FOCUS,
                "max-h-48",
                "overflow-auto",
                "rounded-md",
                "border",
                "border-slate-200",
                "bg-slate-50",
                "p-2",
                "font-mono",
                "text-[10px]",
                "leading-relaxed",
            ],
            id="build-log",
            role="region",
            aria_label="Build output",
            tabindex="0",
        ):
            log_window(build, snapshot.epoch, 0, follow, view)


def connection(snapshot: Snapshot, view: View) -> None:
    if view.transport == "sse" and not snapshot.complete:
        with tag.div(id="changes", hx_swap="none"):
            hx.connect(f"{BASE}/events", close_on="campaign-complete")


def log_page(
    build: Build, snapshot: Snapshot, after: int, follow: bool, view: View
) -> None:
    with tag.main([FRAME, "py-3"], id="workspace"):
        with tag.a(LINK, href=f"/builds/{build.id}?{view.query()}"):
            text(f"← {build.name}")
        with tag.section([PANEL, "mt-2", "p-3"], id="build-detail"):
            with tag.h1([HEADING, "mb-2"]):
                text(f"{build.name}: build output")
            with tag.div(["font-mono", "text-xs"], id="log-chunk"):
                log_window(build, snapshot.epoch, after, follow, view)


def dashboard(snapshot: Snapshot, view: View) -> None:
    with tag.main([FRAME, "py-3"], id="workspace", tabindex="-1"):
        connection(snapshot, view)
        with tag.div(
            [
                "mb-3",
                "flex",
                "flex-col",
                "gap-2",
                "md:flex-row",
                "md:items-center",
                "md:justify-between",
            ]
        ):
            with tag.div():
                with tag.h1(
                    [
                        "text-xl",
                        "font-semibold",
                        "tracking-tight",
                    ]
                ):
                    text("Native build campaign")
            with tag.div(["grid", "shrink-0", "gap-2"]):
                with tag.nav(
                    [
                        "flex",
                        "w-fit",
                        "rounded-lg",
                        "border",
                        "border-slate-200",
                        "bg-slate-100",
                        "p-1",
                    ],
                    aria_label="Update transport",
                ):
                    for mode, title in (
                        ("sse", "SSE + GET"),
                        ("poll", "Polling"),
                    ):
                        url = replace(view, transport=mode).url()
                        with tag.a(
                            [
                                FOCUS,
                                "rounded",
                                "px-3",
                                "py-1",
                                "text-xs",
                                ["bg-white", "text-blue-700", "shadow-sm"]
                                if view.transport == mode
                                else MUTED,
                            ],
                            href=url,
                            aria_current="page"
                            if view.transport == mode
                            else None,
                        ):
                            navigation(url)
                            text(title)
        summary(snapshot, view)
        with tag.div(
            [
                "mt-3",
                "grid",
                "items-start",
                "gap-3",
                "lg:grid-cols-[minmax(0,1fr)_320px]",
            ]
        ):
            with tag.section(PANEL, aria_label="Build inventory"):
                with tag.div([PANEL_TITLE, "px-3", "pt-2", "pb-1"]):
                    with tag.div():
                        with tag.h2(HEADING):
                            text("Build inventory")
                    updates(snapshot, view, snapshot.revision)
                with tag.form(
                    [
                        "filters",
                        "flex",
                        "flex-wrap",
                        "gap-2",
                        "px-3",
                        "py-2",
                    ],
                    method="get",
                    action=BASE,
                ):
                    navigation(BASE)
                    with tag.input(
                        type="hidden",
                        name="transport",
                        value=view.transport,
                    ):
                        pass
                    with tag.label(
                        ["min-w-32", "flex-1", "max-sm:basis-full"]
                    ):
                        with tag.span("sr-only"):
                            text("Search packages")
                        with tag.input(
                            [FIELD, "w-full"],
                            type="search",
                            name="q",
                            value=view.q,
                            placeholder="Search packages…",
                            maxlength="80",
                        ):
                            pass
                    with tag.label():
                        with tag.span("sr-only"):
                            text("Build state")
                        with tag.select(FIELD, name="state"):
                            for state in ("", *STATES):
                                with tag.option(
                                    value=state,
                                    selected=state == view.state,
                                ):
                                    text(
                                        state.title()
                                        if state
                                        else "All states"
                                    )
                    with tag.button([BUTTON, "bg-slate-50"], type="submit"):
                        text("Filter")
                filtered = [
                    b
                    for b in snapshot.builds
                    if view.q.casefold() in b.name.casefold()
                    and (not view.state or view.state == b.state)
                ]
                start = (view.page - 1) * PAGE_SIZE
                rows = filtered[start : start + PAGE_SIZE]
                with tag.div("overflow-x-auto"):
                    with tag.table(
                        [
                            "w-full",
                            "text-left",
                            "text-xs",
                            "whitespace-nowrap",
                        ],
                        id="builds",
                        data_revision=snapshot.revision,
                    ):
                        with tag.caption("sr-only"):
                            text("Builds at the last inventory refresh")
                        with tag.thead(
                            ["border-y", "border-slate-200", "bg-slate-50"]
                        ):
                            with tag.tr():
                                for title in (
                                    "Build",
                                    "Package",
                                    "State",
                                    "Last observation",
                                ):
                                    with tag.th(
                                        [
                                            TABLE_HEADING,
                                            "px-3",
                                            "py-1.5",
                                            ["hidden", "xl:table-cell"]
                                            if title == "Last observation"
                                            else None,
                                        ],
                                        scope="col",
                                    ):
                                        text(title)
                        with tag.tbody(
                            [
                                "divide-y",
                                "divide-slate-100",
                                "[&_td]:px-3",
                                "[&_td]:py-1",
                            ]
                        ):
                            for build in rows:
                                with tag.tr(
                                    "hover:bg-blue-50/40",
                                    id=f"build-row-{build.id}",
                                ):
                                    with tag.td(
                                        ["font-mono", "text-slate-400"]
                                    ):
                                        text(f"{build.id:03}")
                                    with tag.td():
                                        url = f"/builds/{build.id}?transport={view.transport}"
                                        with tag.a(
                                            [
                                                LINK,
                                                "package-link",
                                                "font-semibold",
                                            ],
                                            href=url,
                                        ):
                                            hx.preview(
                                                url, region="#build-detail"
                                            )
                                            text(build.name)
                                    with tag.td():
                                        label(
                                            build.state.title(), build.state
                                        )
                                    with tag.td(
                                        [MUTED, "hidden", "xl:table-cell"]
                                    ):
                                        text(build.phase)
                if not rows:
                    with tag.p([MUTED, "px-3", "py-3"]):
                        text(
                            "No builds match this view. Try another filter or an earlier page."
                        )
                with tag.nav(
                    [
                        PANEL_TITLE,
                        "border-t",
                        "border-slate-200",
                        "px-3",
                        "py-2",
                    ],
                    aria_label="Inventory pages",
                ):
                    with tag.span(SMALL):
                        text(
                            f"{start + 1 if rows else 0}–{start + len(rows) if rows else 0} of {len(filtered)} builds"
                        )
                    with tag.div(["flex", "gap-5", "text-xs"]):
                        for page, title, enabled in (
                            (view.page - 1, "← Previous", view.page > 1),
                            (
                                view.page + 1,
                                "Next →",
                                start + PAGE_SIZE < len(filtered),
                            ),
                        ):
                            if enabled:
                                url = replace(view, page=page).url()
                                with tag.a(LINK, href=url):
                                    navigation(url)
                                    text(title)
                with tag.p(
                    [
                        SMALL,
                        "border-t",
                        "border-slate-200",
                        "bg-slate-50/50",
                        "px-3",
                        "py-1.5",
                    ]
                ):
                    text("Rows update only when you refresh the inventory.")
            chosen = next(
                (b for b in snapshot.builds if b.state == "running"),
                snapshot.builds[0],
            )
            detail(chosen, snapshot, view)
        with tag.span(
            [
                "htmx-indicator",
                "fixed",
                "bottom-5",
                "left-1/2",
                "-translate-x-1/2",
                "rounded-md",
                "bg-slate-800",
                "px-4",
                "py-2",
                "text-white",
            ],
            id="loading",
            role="status",
        ):
            text("Loading view…")
