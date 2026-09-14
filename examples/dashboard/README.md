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
The checked-in CSS is ready to serve; Node is needed to change styles or run
browser regression tests, not to run the application.

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

## Cookbook modules: candidates for Tagflow itself

Two small modules hold the glue that is not about campaigns. They are
prototypes for possible `tagflow.htmx` and `tagflow.starlette` helpers, kept in
the example until they prove themselves here.

`hx.py` names one htmx 4 reading contract per function and sets the matching
attributes on the current Tagflow element: `navigate` (same-page navigation that
pushes history and updates the title), `preview` (load a region into the same
region, changing neither), `refresh` (a self-refreshing embedded representation
that morphs in place and stops when `done`), `read_cursor` (replace yourself with
the next page; keep retrying on an interval and on click; never cancel a slow
response), `connect` (an SSE source for named events), and `recover_reader`
(response headers that replace the reader which asked, resolved with `closest`,
never by global ID). Each docstring records the bug the bundle prevents. The
generated attributes stay visible in the HTML and `attr()` remains available.

`responses.py` provides `render_response(request, component, *, cache_control,
doctype, conditional, headers, status_code)`: render once inside an isolated
document, hash the actual bytes into a strong ETag, and answer a matching GET or
HEAD `If-None-Match` with 304. Entity tags are parsed as quoted tags with weak
comparison, not split on commas. Cache policy is a required argument; `app.py`'s
`representation()` is where this demo decides that its data is public. Recovery
responses pass `conditional=False` with `no-store` and get no validator.

`test/test_dashboard_cookbook.py` pins these contracts independently of the app.
The rule for promoting a helper into the library: one of the browser
failure/reordering regressions must be expressible as a Tagflow integration test
while the corresponding application glue is deleted. Saving `with` statements or
hiding endpoint names is not enough. Not proposed: viewer sessions, a DOM-patch
protocol over SSE, automatic query-parameter forwarding, or a helper that claims
to guarantee an SSE connection survives region swaps; placement of `connect`
remains an application decision checked by the browser suite.

## HTML resource design

| Resource | Representation / behavior |
| --- | --- |
| `/campaigns/native?q=&state=&page=&transport=` | Full page, GET filters, held inventory and navigation |
| `/campaigns/native/summary` | Automatically refreshed summary HTML |
| `/campaigns/native/updates?seen=…` | Notice linking to an explicit inventory refresh |
| `/campaigns/native/events` | Named SSE revision hints, not rendered DOM mutations |
| `/builds/{id}` | Full, independently addressable build page |
| `/builds/{id}/status` | Build status HTML |
| `/builds/{id}/log?epoch=…&after=…&follow=…&transport=…` | Full log page containing at most five lines and the next cursor link |

Links and forms work without JavaScript. htmx enhances navigation using `hx-select`
to extract the desired region from the **same full-page response**. No `HX-Request`
cache variant is needed. Live summaries use htmx 4's `outerMorph`; bounded log pages
replace their cursor control rather than blindly appending a retried response.
Log responses are full, titled pages too; enhanced readers select the chunk's
children, while ordinary navigation retains a heading and link back to the build.
Preview-only swaps ignore response titles, keeping the tab title aligned with its URL.
Pause/resume rebuilds the small log reader from its first page. A changed process
epoch or invalidated offset replaces the reader instead of reusing an old cursor.
Reset targeting is relative to the requesting reader: a delayed response must not
reacquire a newer panel by its global ID. Following polls recur after failures;
overlapping ticks are dropped rather than cancelling a slow response. Clicking the
cursor retries immediately. No application JavaScript is needed for these policies.

HTML responses have strong ETags derived from the actual representation bytes and
`Cache-Control: public, no-cache`: they can be stored but must be revalidated.
Matching conditional GETs return 304. Rendering still occurs before hashing; a
production system could avoid that work with a trustworthy representation version.
Do not substitute a global event cursor for such a version.
Browser revalidation normally exposes a cached 200 body to Fetch after a wire-level
304. This saves transfer, but does not necessarily avoid htmx processing or swapping.

SSE only says **something changed**. Its named `campaign-changed` event triggers
ordinary HTML GETs. Each connection announces the current revision, so reconnects
repair gaps without replaying a mutation history. The extension closes on campaign
completion. SSE mode also polls every 15 seconds as a recovery path; polling mode
uses three seconds and needs no event stream. Active logs check every two seconds.
Standalone build pages establish their own SSE connection outside the replaceable
detail panel. Cursor links and resets retain the selected transport.
Completed resources stop automatic checks. SSE is `no-store`, with proxy buffering
disabled; deploy behind a proxy that supports streaming and suitable idle timeouts.

Each request captures an immutable snapshot; separate requests may see different
revisions. The inventory deliberately does not follow the overview until the reader
chooses “Updates available”. That is a reading policy, not per-viewer server state.
The dashboard detail is deliberately a preview: inventory navigation resets its
selection and following state. Use a build's permalink for a focused reader. A
project that needs persistent dashboard selection should encode it in the URL,
not introduce server-side viewer sessions.

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
npm --prefix examples/dashboard ci
npm exec --prefix examples/dashboard -- playwright install chromium
npm --prefix examples/dashboard test
```

Integration tests exercise both AnyIO backends, ETags, filters, log cursors/reset,
stream reconnect/completion, and bounded lifespan shutdown. For browser checks,
compare summary and inventory revisions before/after an update, switch transports,
filter and navigate back, open a build, and pause/resume its log. The automated
Chromium suite uses a separate deterministic server on port 8011, injects failed
and slow requests, holds reset responses across selection/pause/navigation, checks
EOF stops polling, and exercises log navigation with JavaScript disabled. It never
adds test-control endpoints to the demo. Orb setup prepares its pinned browser.

The local htmx files are vendored from `htmx.org@4.0.0` on jsDelivr:
`dist/htmx.min.js` and `dist/ext/hx-sse.min.js`; their license is in
`static/HTMX-LICENSE`. See [htmx 4](https://four.htmx.org/docs/) and
[the SSE extension](https://four.htmx.org/extensions/hx-sse/).
Tailwind is pinned by `package-lock.json`; generated CSS retains its MIT banner.
