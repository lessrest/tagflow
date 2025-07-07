(defpackage :tagflow-tests
  (:use :cl :fiveam :tagflow :tagflow.html))

(in-package :tagflow-tests)

(def-suite :tagflow-tests
  :description "Test suite for tagflow HTML DSL")

(in-suite :tagflow-tests)

;;; Test helper macros
(defmacro is-html (expected-html &body tagflow-body)
  "Test that TAGFLOW-BODY renders to EXPECTED-HTML"
  `(is (equal ,(if (and (listp expected-html) 
                        (every #'stringp expected-html))
                   `(format nil "~{~a~^~%~}" (list ,@expected-html))
                   expected-html)
              (render-html (document ,@tagflow-body)))))

(defmacro define-html-tests (test-name description &body test-cases)
  "Define multiple HTML rendering test cases in a single test"
  `(test ,test-name ,description
     ,@(loop for test-case in test-cases
             collect (if (stringp (first test-case))
                         `(is-html ,(first test-case) ,@(rest test-case))
                         `(is-html ,(first test-case) ,@(rest test-case))))))

(define-html-tests basic-element-rendering
  "Test basic HTML element rendering"
  ("<div></div>" (<div>))
  (("<p>" "Hello" "</p>") (<p> "Hello"))
  (("<div>" "<p>" "Nested" "</p>" "</div>") (<div> (<p> "Nested"))))

(define-html-tests attribute-rendering
  "Test HTML attribute rendering"
  ("<div id=test></div>" (<div> :id "test"))
  ("<div class=\"foo bar\"></div>" (<div> (classes :foo :bar)))
  ("<div id=test class=box></div>" (<div> :id "test" (classes :box))))

(define-html-tests self-closing-tags
  "Test self-closing HTML tags"
  ("<br>" (<br>))
  ("<hr>" (<hr>))
  ("<img src=test.jpg alt=Test>" (<img> :src "test.jpg" :alt "Test"))
  ("<input type=text name=username>" (<input> :type "text" :name "username"))
  ("<meta charset=UTF-8>" (<meta> :charset "UTF-8"))
  ("<link rel=stylesheet href=style.css>" (<link> :rel "stylesheet" :href "style.css")))

(define-html-tests html-escaping
  "Test HTML special character escaping"
  (("<p>" "&lt;script&gt;alert(&quot;XSS&quot;)&lt;/script&gt;" "</p>") (<p> "<script>alert(\"XSS\")</script>"))
  ("<div title=\"&quot;quoted&quot; &amp; &lt;tagged&gt;\"></div>" (<div> :title "\"quoted\" & <tagged>"))
  (("<p>" "Tom &amp; Jerry" "</p>") (<p> "Tom & Jerry")))

(define-html-tests complex-nesting
  "Test complex nested structures"
  (("<ul>" "<li>" "Item 1" "</li><li>" "Item 2" "</li><li>" "Item 3" "</li>" "</ul>")
   (<ul>
    (<li> "Item 1")
    (<li> "Item 2")
    (<li> "Item 3")))
  (("<div>" "<h1>" "Title" "</h1><p>" "Paragraph with <a href=#>link</a>" "</p>" "</div>")
   (<div>
    (<h1> "Title")
    (<p> "Paragraph with " (<a> :href "#" "link")))))

(define-html-tests multiple-attributes
  "Test elements with multiple attributes"
  ("<a href=https://example.com target=_blank rel=noopener>Link</a>"
   (<a> :href "https://example.com" 
        :target "_blank" 
        :rel "noopener" 
        "Link"))
  ("<input type=email name=email placeholder=\"Enter email\" required=true>"
   (<input> :type "email"
            :name "email"
            :placeholder "Enter email"
            :required "true")))

(define-html-tests dataset-attributes
  "Test data-* attributes"
  ("<div data-id=123></div>" (<div> (dataset "id" "123")))
  ("<button data-action=submit data-target=form1>Submit</button>"
   (<button> 
    (dataset "action" "submit")
    (dataset "target" "form1")
    "Submit")))

(define-html-tests text-and-elements-mixed
  "Test mixing text and elements"
  (("<p>" "Hello <strong>world</strong>!" "</p>")
   (<p> "Hello " (<strong> "world") "!"))
  (("<div>" "Start<br>Middle<br>End" "</div>")
   (<div> "Start" (<br>) "Middle" (<br>) "End")))

(define-html-tests empty-attributes
  "Test handling of empty/nil attributes"
  ("<div></div>" (<div> :id nil))
  ("<div class=\"\"></div>" (<div> :class "")))

(define-html-tests semantic-html5-tags
  "Test HTML5 semantic tags"
  (("<header>" "<nav>" "<a href=/>Home</a>" "</nav>" "</header>")
   (<header>
    (<nav>
     (<a> :href "/" "Home"))))
  (("<main>" "<article>" "<h2>" "Title" "</h2><p>" "Content" "</p>" "</article>" "</main>")
   (<main>
    (<article>
     (<h2> "Title")
     (<p> "Content"))))
  (("<footer>" "<p>" "Copyright 2024" "</p>" "</footer>")
   (<footer>
    (<p> "Copyright 2024"))))

(define-html-tests form-elements
  "Test form-related elements"
  ("<textarea name=message rows=4 cols=50>Default text</textarea>"
   (<textarea> :name "message" :rows "4" :cols "50" "Default text"))
  (("<button type=submit>Click me</button>")
   (<button> :type "submit" "Click me")))

(define-html-tests code-and-pre-tags
  "Test code and pre tags"
  (("<pre>" "<code>function hello() { return &quot;world&quot;; }</code>" "</pre>")
   (<pre>
    (<code> "function hello() { return \"world\"; }"))))

(define-html-tests heading-tags
  "Test all heading levels"
  (("<h1>" "Heading 1" "</h1>") (<h1> "Heading 1"))
  (("<h2>" "Heading 2" "</h2>") (<h2> "Heading 2"))
  (("<h3>" "Heading 3" "</h3>") (<h3> "Heading 3"))
  (("<h4>" "Heading 4" "</h4>") (<h4> "Heading 4"))
  (("<h5>" "Heading 5" "</h5>") (<h5> "Heading 5"))
  (("<h6>" "Heading 6" "</h6>") (<h6> "Heading 6")))

(define-html-tests complete-document-structure
  "Test complete HTML document structure"
  (("<html>" 
	"<head>"
	"<title>Test Page</title><meta charset=UTF-8><link rel=stylesheet href=style.css>" 
	"</head><body>" 
	"<h1>" "Welcome" "</h1><p>" "Hello, world!" "</p>" 
	"</body>" "</html>")
   (<html>
    (<head>
     (<title> "Test Page")
     (<meta> :charset "UTF-8")
     (<link> :rel :stylesheet :href "style.css"))
    (<body>
     (<h1> "Welcome")
     (<p> "Hello, world!")))))

;; Run tests with (fiveam:run! :tagflow-tests)
