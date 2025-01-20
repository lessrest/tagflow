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
          with tag.script(src="https://cdn.tailwindcss.com"):
              pass # no content in the script tag

      # use "classes" to avoid conflict with the "class" keyword
      with tag.body(classes="bg-gray-50 p-4"):
          with tag.h1():
              text("Welcome!")
          # positional arguments are also used as class names
          with tag.p("serif", "mb-4"):
              # another way to set attributes
              attr("contenteditable")
              attr("spellcheck", False)
              # classes can also be an iterable
              attr("class", ["text-lg text-gray-900", "font-bold"])
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
@html.article(classes="w-prose mx-auto")
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
            with tag.script(src="https://cdn.tailwindcss.com"):
                pass

        with tag.body(classes="bg-gray-50 p-4"):
            yield

@contextmanager
def article():
    with tag.article(classes="w-prose mx-auto"):
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
                with tag.p(classes="text-red-500"):
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
        with tag.p(classes="mb-4"):
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
from trio import sleep

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
                        await sleep(1)
                        i += 1

                # We can spawn a task in the session's task scope.
                # All session tasks are cancelled when the session is closed.
                spawn(loop)
```

This feature is currently based on Trio for structured concurrency with
nurseries. It works with the Hypercorn ASGI server and FastAPI. It might be nice
to change it into a plain ASGI middleware; maybe also supporting other async
systems via `anyio`.

It remains to be seen whether the "session" concept makes sense, and how to
think about session lifecycles, reconnects, etc.

## License

Tagflow is open source software released under the MIT license. See the
[LICENSE](LICENSE) file for more details.
