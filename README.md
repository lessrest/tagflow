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
This is an alternative to—not a replacement for—the WebSocket live-document API
below. Two small optional modules carry the parts of that design that are not
about any particular dashboard.

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

## Live Documents (early working prototype)

Tagflow also offers "live documents" that asynchronous server tasks can update
in real time after the initial page load. It works a bit like Phoenix LiveView:
the browser runs a script that exposes a DOM mutation capability via WebSocket,
letting the server send updates to elements in the document.

Let's look at a simple example of a live document that updates a counter.

```python
from tagflow import tag, text, document, clear, spawn, transition
from tagflow import TagResponse, DocumentMiddleware, Live

from contextlib import asynccontextmanager

from fastapi import FastAPI
import anyio

live = Live()

app = FastAPI(lifespan=live.run)
app.add_middleware(DocumentMiddleware)

@app.get("/counter", response_class=TagResponse)
async def counter():
    session = await live.session()

    with tag.html():
        with tag.body():
            live.script_tag()
            session.client_tag()

            with tag.h1():
                async def loop():
                    i = 0
                    while True:
                        # A transition is like a transaction.
                        # After the block exits, the change is sent via WebSocket.
                        # The browser script applies it using a DOM View Transition.
                        with transition():
                            # This applies to the H1 element which is the current node.
                            clear()
                            text(str(i))
                        await anyio.sleep(1)
                        i += 1

                # We can spawn a task in the session's task scope.
                # All session tasks are cancelled when the session is closed.
                spawn(loop)
```

This feature uses AnyIO for structured concurrency and can work with either
`asyncio` or `trio` as the backend.

It remains to be seen whether the "session" concept makes sense, and how to
think about session lifecycles, reconnects, etc.

## License

Tagflow is open source software released under the MIT license. See the
[LICENSE](LICENSE) file for more details.
