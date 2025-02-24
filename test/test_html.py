import pytest

from tagflow import (
    XMLResponse,
    TagResponse,
    tag,
    attr,
    html,
    text,
    classes,
    dataset,
    document,
)


@pytest.fixture
def doc():
    """Create a new document for testing"""
    with document() as d:
        yield d


def test_basic_document():
    """Test creating a basic document with tags and text"""
    with document() as doc:
        with tag("div"):
            with tag("p"):
                text("Hello World")

    result = doc.to_html(compact=True)
    assert "<div><p>Hello World</p></div>" in result


def test_html_decorators():
    """Test the @html decorator functionality"""

    @html.div(class_="container")
    def my_component():
        with tag("p"):
            text("Content")

    with document() as doc:
        my_component()

    result = doc.to_html(compact=True)
    assert '<div class="container"><p>Content</p></div>' in result


def test_attributes():
    """Test setting various HTML attributes"""
    with document() as doc:
        with tag("input", type="text", required=True, disabled=False):
            pass
        with tag("div", tabindex=1, data_count=42):
            pass

    result = doc.to_html(compact=True)
    assert 'type="text"' in result
    assert "required" in result
    assert "disabled" not in result
    assert 'tabindex="1"' in result
    assert 'data-count="42"' in result

    # Test invalid attribute types
    with pytest.raises(TypeError) as exc_info:
        with document() as doc:
            with tag("div", data_obj={"foo": "bar"}):
                pass
    assert (
        "Attribute values must be strings, numbers, booleans, or lists"
        in str(exc_info.value)
    )
    assert "dict" in str(exc_info.value)


def test_classes():
    """Test adding CSS classes with nested lists and merging behavior"""
    with document() as doc:
        # Test basic class addition
        with tag("div"):
            classes("foo", "bar")
            text("Content")

        # Test nested lists in classes function
        with tag("div"):
            classes(["a", ["b", "c"]], "d")
            text("Content")

        # Test empty values are filtered
        with tag("div"):
            classes("x", "", ["y", None, ["", "z"]])
            text("Content")

    result = doc.to_html(compact=True)
    assert '<div class="foo bar">Content</div>' in result
    assert '<div class="a b c d">Content</div>' in result
    assert '<div class="x y z">Content</div>' in result


def test_tag_class_merging():
    """Test class merging behavior in tag creation"""
    with document() as doc:
        with tag("div", "one", ["two", "three"]):
            text("A")

        with tag("div", "one", ["two"], class_="override"):
            text("B")

        with tag("div", "one", classes=["two", ["three", "four"]]):
            text("C")

        with tag("div", ["a", ["b", "c"]], ["d"], class_="e f"):
            text("D")

    result = doc.to_html(compact=True)
    assert '<div class="one two three">A</div>' in result
    assert '<div class="one two override">B</div>' in result
    assert '<div class="one two three four">C</div>' in result
    assert '<div class="a b c d e f">D</div>' in result


def test_dataset():
    """Test adding data attributes"""
    with document() as doc:
        with tag("div"):
            dataset({"test": "value", "other": "123"})

    result = doc.to_html()
    assert 'data-test="value"' in result
    assert 'data-other="123"' in result


def test_nested_tags():
    """Test nesting multiple tags"""
    with document() as doc:
        with tag("div"):
            with tag("header"):
                text("Title")
            with tag("main"):
                with tag("p"):
                    text("Content")
            with tag("footer"):
                text("Footer")

    expected = """\
<div>
 <header>
  Title
 </header>
 <main>
  <p>
   Content
  </p>
 </main>
 <footer>
  Footer
 </footer>
</div>
"""

    assert doc.to_html(compact=False) == expected


def test_fragment_xml():
    """Test XML serialization of fragments"""
    with document() as doc:
        with tag("root"):
            with tag("child"):
                text("content")

    xml = doc.to_xml()
    assert "<root><child>content</child></root>" in xml


def test_hypermedia_response():
    """Test TagResponse rendering"""
    with document():
        with tag("div"):
            text("Test")
        response = TagResponse(content=None)
        rendered = response.render(None)
        assert b"<div>Test</div>" in rendered


def test_xml_response():
    """Test XMLResponse rendering"""
    with document():
        with tag("root"):
            text("Test")
        response = XMLResponse(content=None)
        rendered = response.render(None)
        assert b"<root>Test</root>" in rendered


def test_attr_modification():
    """Test modifying attributes after creation"""
    with document() as doc:
        with tag("div"):
            attr("id", "test")
            attr("class", "foo")
            attr("data-test", None)  # Should remove attribute
            text("Content")

    result = doc.to_html()
    assert 'id="test"' in result
    assert 'class="foo"' in result
    assert "data-test" not in result


def test_multiple_text_nodes():
    """Test adding multiple text nodes"""
    with document() as doc:
        with tag("p"):
            text("Hello")
            text(" ")
            text("World")

    result = doc.to_html()
    assert "<p>Hello World</p>" in result


def test_exact_html_document():
    """Test creating a complete HTML document with exact output matching"""
    with document() as doc:
        with tag("html"):
            with tag("head"):
                with tag("title"):
                    text("Test Page")
                with tag("meta", charset="utf-8"):
                    pass
            with tag("body", class_="content"):
                with tag("header"):
                    with tag("h1"):
                        text("Welcome")
                with tag("main"):
                    with tag("p", class_="intro"):
                        text("This is a ")
                        with tag("strong"):
                            text("test")
                        text(" page.")
                    with tag("ul"):
                        for item in ["One", "Two", "Three"]:
                            with tag("li"):
                                text(item)

    expected = """\
<html>
 <head>
  <title>
   Test Page
  </title>
  <meta charset="utf-8"/>
 </head>
 <body class="content">
  <header>
   <h1>
    Welcome
   </h1>
  </header>
  <main>
   <p class="intro">
    This is a
    <strong>
     test
    </strong>
    page.
   </p>
   <ul>
    <li>
     One
    </li>
    <li>
     Two
    </li>
    <li>
     Three
    </li>
   </ul>
  </main>
 </body>
</html>
"""

    assert doc.to_html(compact=False) == expected


def test_html_decorator_variations():
    """Test different ways of using the HTMLDecorators API with nested classes"""

    @html.div(["container", ["nested", "classes"]])
    def container():
        with tag("h1"):
            text("Main Title")

    @html("section", ["content", ["section"]], data_role="content")
    def section():
        with tag("p"):
            text("Section content")

    @html.article(id="post-1", class_=["blog", ["post", "featured"]])
    def article():
        container()
        section()
        with tag("footer"):
            text("Article footer")

    with document() as doc:
        article()

    result = doc.to_html(compact=True)

    # Check the outer article element
    assert '<article id="post-1" class="blog post featured">' in result
    # Check the nested container
    assert (
        '<div class="container nested classes"><h1>Main Title</h1></div>'
        in result
    )
    # Check the section with multiple attributes
    assert 'class="content section"' in result
    assert 'data-role="content"' in result
    assert "<p>Section content</p>" in result
    # Check the footer
    assert "<footer>Article footer</footer>" in result
