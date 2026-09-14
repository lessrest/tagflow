# Tagflow campaign dashboard

A simulated build-campaign dashboard using async Starlette, Tagflow, htmx **4.0.0**,
and Tailwind **4.3.3**. Inspired by the Filnix campaign monitor's shared progress,
held inventories, and incremental logs, without coupling this example to Nix.

## Run

From the repository root:

```sh
uv sync --locked --all-extras
uv run --no-sync hypercorn examples.dashboard.app:app --bind 0.0.0.0:8000
```

Open `/campaigns/native` on that server. In an Amp orb, use the supervised command
in [the orb guide](../../.agents/README.md) and its printed portal URL instead.
No credentials, database, Nix installation, or external asset service is needed.
The checked-in CSS is ready to serve; Node is only needed to change styles.

One shared simulation advances every four seconds and finishes after about two
minutes. Reloading a browser does not restart it; restart the server to replay.
Run one worker: this intentionally tiny in-memory fixture is not a shared database.
The application has no viewer cookies or sessions, and no mutation endpoints.

## Tailwind, the Tagflow way

All dashboard styling is expressed in utility tokens in `views.py` and `app.py`.
Reusable groups such as `PANEL`, `FOCUS`, and `BUTTON` are typed nested lists, not
CSS component classes. Compose them directly:

```python
with tag.div([PANEL, ["px-5", "py-4"], "lg:col-span-2"]):
    text("Shared resources, independent readers")
```

Use complete literal tokens in conditional branches so Tailwind can discover
them; do not interpolate class names like `f"text-{color}-700"`.
`tailwind.css` only imports Tailwind and declares the Python source files to scan.
`static/dashboard.css` is generated—do not edit it manually.

```sh
npm --prefix examples/dashboard ci
npm --prefix examples/dashboard run build
# During styling work, in another terminal:
npm --prefix examples/dashboard run watch
```

## HTML resource design

| Resource | Representation / behavior |
| --- | --- |
| `/campaigns/native?q=&state=&page=&transport=` | Full page, GET filters, held inventory and navigation |
| `/campaigns/native/summary` | Automatically refreshed summary HTML |
| `/campaigns/native/updates?seen=…` | Notice linking to an explicit inventory refresh |
| `/campaigns/native/events` | Named SSE revision hints, not rendered DOM mutations |
| `/builds/{id}` | Full, independently addressable build page |
| `/builds/{id}/status` | Build status HTML |
| `/builds/{id}/log?epoch=…&after=…&follow=…` | At most five log lines plus the next cursor link |

Links and forms work without JavaScript. htmx enhances navigation using `hx-select`
to extract the desired region from the **same full-page response**. No `HX-Request`
cache variant is needed. Live summaries use htmx 4's `outerMorph`; bounded log pages
replace their cursor control rather than blindly appending a retried response.
Pause/resume rebuilds the small log reader from its first page. A changed process
epoch or invalidated offset replaces the reader instead of reusing an old cursor.

HTML responses have strong ETags derived from the actual representation bytes and
`Cache-Control: public, no-cache`: they can be stored but must be revalidated.
Matching conditional GETs return 304. Rendering still occurs before hashing; a
production system could avoid that work with a trustworthy representation version.
Do not substitute a global event cursor for such a version.

SSE only says **something changed**. Its named `campaign-changed` event triggers
ordinary HTML GETs. Each connection announces the current revision, so reconnects
repair gaps without replaying a mutation history. The extension closes on campaign
completion. SSE mode also polls every 15 seconds as a recovery path; polling mode
uses three seconds and needs no event stream. Active logs check every two seconds.
Completed resources stop automatic checks. SSE is `no-store`, with proxy buffering
disabled; deploy behind a proxy that supports streaming and suitable idle timeouts.

Each request captures an immutable snapshot; separate requests may see different
revisions. The inventory deliberately does not follow the overview until the reader
chooses “Updates available”. That is a reading policy, not per-viewer server state.

This demonstrates an alternative to Tagflow's existing WebSocket live documents,
which remain useful for stateful interactions. It does not add a general-purpose
live-update protocol to the library, implement long polling, or connect to Filnix.
A real campaign backend would need durable snapshots, stable ordering/keyset
pagination, bounded byte-oriented logs, and shared notification infrastructure.
Private data would also require different cache and authorization policies.

## Verification and vendor sources

```sh
uv run --no-sync pytest -q
uv run --no-sync ruff check src test examples
uv run --no-sync ruff format --check src test examples
uv run --no-sync pyright
```

Integration tests exercise both AnyIO backends, ETags, filters, log cursors/reset,
stream reconnect/completion, and bounded lifespan shutdown. For browser checks,
compare summary and inventory revisions before/after an update, switch transports,
filter and navigate back, open a build, and pause/resume its log.

The local htmx files are vendored from `htmx.org@4.0.0` on jsDelivr:
`dist/htmx.min.js` and `dist/ext/hx-sse.min.js`; their license is in
`static/HTMX-LICENSE`. See [htmx 4](https://four.htmx.org/docs/) and
[the SSE extension](https://four.htmx.org/extensions/hx-sse/).
Tailwind is pinned by `package-lock.json`; generated CSS retains its MIT banner.
