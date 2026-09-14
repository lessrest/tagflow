# 🏷️ Tagflow

Tagflow is a Python library that generates HTML in a block-oriented way that
works with ordinary control flow. Instead of nested function calls, it uses
context managers and decorators to align HTML generation with Python's native
language features.

So you can write HTML using familiar Python constructs: loops for repeated
elements, conditionals for dynamic content, and try/except for error boundaries.
Create reusable components with decorators and organize code into reusable
components that yield to produce child content.

Most Python HTML generation libraries follow a pattern of nested call
expressions to mirror HTML's tree structure, using lists, list comprehensions,
and variable arguments to pass children. This forces us to abandon familiar
control flow patterns (`if`, `for`, `continue`, `try`) in favor of
functional-style composition, leading to convoluted code when dealing with
conditional rendering, loops, or error handling.

Tagflow takes a different approach by using context managers to track the
current position in the HTML tree, allowing developers to use ordinary Python
control flow while building HTML documents step by step in nested `with` blocks.

We have not yet measured the performance of Tagflow, but it would be interesting
to see how it compares to other Python HTML generation libraries. It's not
optimized for speed but we haven't seen any major performance issues.

The current implementation uses Python's builtin `ElementTree` for maintaining
the document tree and serializing it to HTML.

While streaming responses aren't implemented yet, Tagflow's design will support
generating and sending HTML incrementally without buffering entire pages in
memory.

## Installation

Tagflow is available on PyPI with the name `tagflow`.

```bash
uv add tagflow
```

## Usage

```python
from tagflow import tag, text, document, attr

def page(title: str):
  # open a <html> tag in the current document
  with tag.html(lang="en"):
      with tag.head():
          with tag.title():
              # append a text node to the title tag
              text(title)
          # elements are appended when created, so an empty element
          # needs no `with` block
          tag.script(src="https://cdn.tailwindcss.com")

      # use "classes" to avoid conflict with the "class" keyword
      with tag.body(classes="bg-gray-50 p-4"):
          with tag.h1():
              text("Welcome!")
          # positional arguments are also used as class names
          with tag.p("serif", "mb-4"):
              # another way to set attributes
              attr("contenteditable")
              attr("spellcheck", False)
              # classes can also be a nested list of strings
              attr("class", ["text-lg text-gray-900", ["font-bold", "italic"]])
              # after emitting content you can't change the attributes
              text("This is a paragraph.")

def index_html() -> str:
  # document() is a context manager that creates a new document root
  with document() as root:
    page("Tagflow")
    return root.to_html()
```

You can use decorators to specify the tag structure for a function. This is a
nice way to define reusable components.

```python
from tagflow import tag, text, html

# decorator nesting lets us skip indentation for basic structure
@html.main(lang="en")
@html.article("w-prose mx-auto", ["font-bold", "italic"], data_role="content")
def welcome(name: str):
    with tag.h1():
        text(f"Welcome, {name}!")
    with tag.p():
        text("This is a paragraph.")
```

To define a component that takes children, just define a context manager. Here
we also show some control flow examples.

```python
from tagflow import tag, text, html
from dataclasses import dataclass
from contextlib import contextmanager

@contextmanager
def page(title: str):
    with tag.html(lang="en"):
        with tag.head():
            with tag.title():
                text(title)
            tag.script(src="https://cdn.tailwindcss.com")

        with tag.body("bg-gray-50 p-4"):
            yield

@contextmanager
def article():
    with tag.article("w-prose mx-auto"):
        yield

@dataclass
class Post:
    id: str
    title: str
    content: list[str]

def index(posts: list[Post]):
    with page("Posts"):
        # just use a loop; no special iteration feature
        for post in posts:
            # just use if; no special blank feature
            if not post.content:
                continue

            # just use try; no special error boundary feature
            try:
                render_post(post)
            except ValueError as e:
                with tag.p("text-red-500"):
                    text("Error rendering post: ")
                    text(str(e))

@html.article()
def render_post(post: Post):
    if not post.id:
        raise ValueError("Post ID is required")

    attr("id", post.id)
    with tag.h1():
        text(post.title)

    for block in post.content:
        with tag.p("mb-4"):
            text(block)
```

## FastAPI response class

Tagflow provides a custom FastAPI response class and middleware that make it
easy to integrate with FastAPI endpoints. The middleware automatically sets up a
fresh document context for each request, while the response class handles
rendering:

```python
from fastapi import FastAPI
from tagflow import tag, text, DocumentMiddleware, TagResponse

app = FastAPI()
app.add_middleware(DocumentMiddleware)

@app.get("/", response_class=TagResponse)
def home():
    with tag.html(lang="en"):
        with tag.head():
            with tag.title():
                text("Home")
        with tag.body():
            with tag.h1():
                text("Welcome!")

# Works with async endpoints too
@app.get("/posts/{id}", response_class=TagResponse)
async def view_post(id: str):
    post = await get_post(id)
    with tag.html():
        with tag.body():
            render_post(post)
```

You can also set the FastAPI default response class to `TagResponse` in your
FastAPI app.

```python
from fastapi import FastAPI
from tagflow import TagResponse

app = FastAPI(default_response_class=TagResponse)

@app.get("/")
def home():
    with tag.html():
        with tag.body():
            with tag.h1():
                text("Welcome!")
```

## Resource-oriented live dashboards

The [campaign dashboard example](examples/dashboard/README.md) combines Tagflow,
async Starlette, htmx 4, and compiled Tailwind utilities. Ordinary HTML resources
support filtering, pagination, build permalinks, and cursor-based logs. Choose
named SSE notifications followed by GETs, or polling the same resources.
The overview updates automatically; the inventory stays still until you refresh it.
There are no viewer sessions, application JSON requests, or handwritten DOM updates.

Styles are composed as nested Tagflow class-token lists, including conditional
utility groups; the `ClassValue` type for such lists is exported from `tagflow`.
The unit of change on that page is a *region*: one element with an `id`,
rendered by one component, morphed in place when new HTML arrives. The
[live sessions](#live-regions-over-a-websocket) below push the same unit over a
WebSocket, so a component written once can refresh itself over HTTP on one page
and be pushed by the server on another. Two small optional modules carry the
parts of the htmx design that are not about any particular dashboard.

### `tagflow.htmx`: named reading contracts

Each function sets htmx 4 attributes on the current element, exactly as
`attr()` would. The point is not fewer lines; it is that each bundle names one
contract and encodes lifecycle rules that are easy to get subtly wrong:

```python
from tagflow import tag, text
from tagflow import htmx as hx

# Same-page navigation: replace #workspace from the same full page the
# server would send anyway, push the URL, update the title.
with tag.a(href=url):
    hx.navigate(url, region="#workspace", indicator="#loading")
    text("Next page")

# Preview: load a region into the same region; no history, no title change.
with tag.a(href=f"/builds/{id}"):
    hx.preview(f"/builds/{id}", region="#build-detail")

# Self-refreshing embedded representation; morphs in place; stops when done.
with tag.section(id="summary"):
    hx.refresh("/summary", trigger="changed from:body, every 15s", done=finished)

# Cursor reader: replace yourself with the next page's selection; retry on an
# interval and on click; never cancel a slow response.
with tag.a(href=next_url):
    hx.read_cursor(next_url, select="#log-chunk > *", every="2s")

# SSE source for named events. Place it outside any region a swap replaces.
with tag.div(hx_swap="none"):
    hx.connect("/events", close_on="complete")

# Response headers that replace the reader which asked, resolved relative to
# the requesting element so a late response cannot reacquire a newer panel.
headers = hx.recover_reader(closest="#build-detail")
```

### `tagflow.responses`: an explicit render-and-respond boundary

With the `starlette` extra, `render_response` renders a component in an
isolated document, hashes the exact bytes into a strong ETag, and answers a
matching `If-None-Match` on GET or HEAD with 304. Cache policy is required:

```python
from tagflow.responses import render_response

async def summary(request):
    return render_response(
        request, lambda: views.summary(state), cache_control="public, no-cache"
    )

# Error or recovery representations: no validator, never stored.
return render_response(
    request, page, cache_control="no-store", conditional=False, headers=headers
)
```

`render(component)` alone is also exported from `tagflow` for callers that
only need the HTML string.

## Live regions over a WebSocket

A `Live` session pushes regions to a page after it has loaded. A region is a
component that renders exactly one element carrying an `id`; `render_region`
enforces that rule and is what `Session.update()` uses. The browser finds the
element by id and morphs it in place with [Idiomorph](https://github.com/bigskysoftware/idiomorph)
(vendored, the algorithm htmx uses), so focus, scroll position, and unchanged
nodes survive.

```python
import anyio
from starlette.applications import Starlette
from starlette.routing import Route
from tagflow import tag, text, Live
from tagflow.responses import render_response

live = Live()

def counter(value: int) -> None:
    with tag.output(id="counter"):
        text(str(value))

async def page(request):
    session = await live.session()

    async def count():
        for i in range(1, 10**6):
            await anyio.sleep(1)
            session.update(lambda: counter(i))   # one message, morphed in place

    def content():
        with tag.html():
            with tag.head():
                live.script_tag()
            with tag.body():
                session.client_tag()   # outside every region
                counter(0)

    session.spawn(count)
    return render_response(request, content, doctype=True, cache_control="no-store")

app = Starlette(lifespan=live.run, routes=[Route("/", page)])
```

What the server keeps is the latest HTML of each region, not a copy of the
browser's DOM. That makes the connection lifecycle simple to state:

- A connection first receives every region it has not seen at its current
  HTML, then one update per change. `update(a, b)` arrives as one message and
  the browser applies it atomically (in a view transition where supported).
- Re-rendering identical HTML sends nothing. Changes made while no browser is
  attached are coalesced, not queued.
- The client reconnects with backoff. If the server no longer knows the session
  (it ended, or the process restarted) it closes with code 4001 and the client
  reloads the page; listen for the cancelable `tagflow:expired` event to do
  something else.
- A session ends when `cancel()` is called, when `Live.run()` shuts down, or
  once no browser has been attached for `Live(grace=…)` seconds (default 30).
  Its `spawn()`ed tasks end with it.

Works with either AnyIO backend. `Live.run(app)` mounts the client under
`/.well-known/tagflow/static/` and the socket at `/.well-known/tagflow/live.ws`
on any Starlette application, FastAPI included.

## License

Tagflow is open source software released under the MIT license. See the
[LICENSE](LICENSE) file for more details.
