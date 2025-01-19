from contextlib import asynccontextmanager, contextmanager
from dataclasses import dataclass
from typing import List

from fastapi import FastAPI
from tagflow import (
    DocumentMiddleware,
    Live,
    TagResponse,
    tag,
    text,
    attr,
    clear,
)

# Initialize our Live instance for WebSocket support
live = Live()


# Sample data structure for our posts
@dataclass
class Post:
    id: str
    title: str
    content: List[str]


# Sample data
POSTS = [
    Post(
        id="1",
        title="Welcome to Tagflow",
        content=[
            "This is a demo of Tagflow's HTML generation capabilities.",
            "It supports regular server-rendered pages and live updates via WebSocket.",
        ],
    ),
    Post(
        id="2",
        title="Live Counter Demo",
        content=[
            "Check out the /counter route to see live updates in action.",
            "The counter updates every second using WebSocket communication.",
        ],
    ),
]


# FastAPI lifespan for managing the Live instance
@asynccontextmanager
async def lifespan(app: FastAPI):
    async with live.run(app):
        yield


# Initialize FastAPI with Tagflow middleware
app = FastAPI(
    lifespan=lifespan,
    default_response_class=TagResponse,
    title="Tagflow Demo",
    description="A demo application showing Tagflow's capabilities",
)
app.add_middleware(DocumentMiddleware)


@contextmanager
def layout(title: str):
    """Common layout wrapper for all pages"""
    with tag.html(lang="en"):
        with tag.head():
            with tag.title():
                text(f"{title} - Tagflow Demo")
            # Add TailwindCSS for styling
            with tag.script(src="https://cdn.tailwindcss.com"):
                pass
            # Add Tagflow client script for live updates
            live.script_tag()

        with tag.body(classes="bg-gray-100 min-h-screen"):
            with tag.nav(classes="bg-white shadow mb-8"):
                with tag.div(classes="max-w-7xl mx-auto px-4 py-4"):
                    with tag.ul(classes="flex space-x-8"):
                        with tag.li():
                            with tag.a(
                                href="/",
                                classes="text-blue-600 hover:text-blue-800",
                            ):
                                text("Home")
                        with tag.li():
                            with tag.a(
                                href="/posts",
                                classes="text-blue-600 hover:text-blue-800",
                            ):
                                text("Posts")
                        with tag.li():
                            with tag.a(
                                href="/counter",
                                classes="text-blue-600 hover:text-blue-800",
                            ):
                                text("Live Counter")

            with tag.main(classes="max-w-7xl mx-auto px-4"):
                yield


def render_post(post: Post):
    """Reusable component for rendering a post"""
    with tag.article(classes="bg-white rounded-lg shadow p-6 mb-6"):
        attr("id", f"post-{post.id}")
        with tag.h2(classes="text-2xl font-bold mb-4"):
            text(post.title)
        for paragraph in post.content:
            with tag.p(classes="mb-4 text-gray-700"):
                text(paragraph)


@app.get("/")
async def home():
    with layout("Home"):
        with tag.div(classes="prose"):
            with tag.h1(classes="text-4xl font-bold mb-8"):
                text("Welcome to Tagflow Demo")
            with tag.p(classes="text-xl text-gray-700"):
                text(
                    "This demo showcases Tagflow's capabilities for building HTML in Python."
                )
            with tag.ul(classes="mt-6"):
                with tag.li():
                    text("Server-rendered pages with clean Python syntax")
                with tag.li():
                    text("Live updates via WebSocket")
                with tag.li():
                    text("Integration with FastAPI and Hypercorn")


@app.get("/posts")
async def posts():
    with layout("Posts"):
        with tag.div():
            with tag.h1(classes="text-4xl font-bold mb-8"):
                text("Posts")
            for post in POSTS:
                render_post(post)


@app.get("/counter")
async def counter():
    session = await live.session()

    with layout("Live Counter"):
        with tag.div(classes="text-center"):
            with tag.h1(classes="text-4xl font-bold mb-8"):
                text("Live Counter Demo")

            with tag.div(
                classes="bg-white rounded-lg shadow p-8 inline-block"
            ):
                # Add the live client element
                live.client_tag(session.id)

                with tag.div(classes="text-6xl font-mono"):

                    async def update_counter():
                        i = 0
                        while True:
                            async with session.transition():
                                clear()
                                text(str(i))
                            await trio.sleep(1)
                            i += 1

                    session.spawn(update_counter)


if __name__ == "__main__":
    import trio
    import hypercorn.trio
    import hypercorn.config
    import logging

    logging.basicConfig(level=logging.INFO)

    config = hypercorn.config.Config()
    config.bind = ["localhost:8000"]
    # Use trio worker class
    config.worker_class = "trio"

    trio.run(
        hypercorn.trio.serve,
        app,
        config,
    )