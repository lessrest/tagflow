(uiop:define-package 
 :tagflow
 (:use :cl :alexandria)
 (:export #:document #:text #:attr #:classes #:dataset
		  #:render-html #:deftag #:defhtmltags #:define-tags
		  #:defattrs #:self-closing-p
		  #:block-element-p))

(in-package :tagflow)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Node hierarchy (attr plist)

(defclass node ()
  ((children :initform nil :accessor children)))

(defclass element (node)
  ((attrs :initform nil :accessor attrs)))

(defclass html-element (element) ())
(defclass root-node   (node)    ())

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Generic dispatch

(defgeneric tag-name-of (obj))
(defmethod tag-name-of ((e html-element))
  (let ((name (symbol-name (class-name (class-of e)))))
    (string-downcase (subseq name 1 (1- (length name))))))
(defmethod tag-name-of ((n root-node)) "root")

(defgeneric self-closing-p (obj))
(defmethod self-closing-p ((e html-element)) nil)

(defgeneric block-element-p (obj))
(defmethod block-element-p ((e html-element)) nil)

(defgeneric add-child (parent child))
(defmethod add-child ((p node) c) (push c (children p)) c)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Utils

;;; plist-put helper  
(defun plist-put (plist key val)
  (loop for (k v) on plist by #'cddr
        for i from 0 by 2
        when (equal k key)
          return (let ((copy (copy-list plist)))
                   (setf (nth (1+ i) copy) val)
                   copy)
        finally (return (append plist (list key val)))))

(defun string-join (items &optional (sep " "))
  (reduce (lambda (a b) (if (string= a "") b (concatenate 'string a sep b)))
          items :initial-value ""))

(defun %flatten-value (v)
  (cond ((null v) "")
        ((stringp v) v)
        ((symbolp v) (string-downcase (symbol-name v)))
        ((listp v)   (string-join (remove "" (mapcar #'%flatten-value v))))
        (t (prin1-to-string v))))

(defun escape-html (s)
  (with-output-to-string (o)
    (loop for ch across s do
      (case ch (#\< (write-string "&lt;" o)) (#\> (write-string "&gt;" o))
                 (#\& (write-string "&amp;" o)) (#\" (write-string "&quot;" o))
                 (t (write-char ch o))))))

;;; Rendering

(defgeneric render (obj stream))
(defmethod render ((s string) stream) (princ (escape-html s) stream))
(defmethod render ((n root-node) stream) (dolist (c (reverse (children n))) (render c stream)))

(defun needs-quotes-p (value)
  "Check if an attribute value needs to be quoted"
  (or (string= value "")
      (position #\Space value)
      (position #\Tab value)
      (position #\Newline value)
      (position #\Return value)
      (position #\' value)
      (position #\" value)
      (position #\= value)
      (position #\< value)
      (position #\> value)
      (position #\` value)))

(defun %render-attrs (plist stream)
  (loop for (k v) on plist by #'cddr
        do (let ((name (if (stringp k) k (string-downcase (symbol-name k))))
                 (val  (%flatten-value v)))
             (when v 
               (if (needs-quotes-p val)
                   (format stream " ~a=\"~a\"" name (escape-html val))
                   (format stream " ~a=~a" name (escape-html val)))))))

(defmethod render ((e html-element) stream)
  (format stream "<~a" (tag-name-of e))
  (%render-attrs (attrs e) stream)
  (if (self-closing-p e)
      (write-string ">" stream)
      (progn (write-char #\> stream)
             (when (and (block-element-p e) (children e))
               (write-char #\Newline stream))
             (dolist (c (reverse (children e))) (render c stream))
             (when (and (block-element-p e) (children e))
               (write-char #\Newline stream))
             (format stream "</~a>" (tag-name-of e)))))

(defun render-html (root) 
  (with-output-to-string (s) (render root s)))

;;; Attribute helper API -------------------------------------------------------

(defvar *current-node* nil)

(defun attr (key &rest value)
  (let ((plist (attrs *current-node*)))
    (setf (attrs *current-node*)
          (plist-put plist key (if (> (length value) 1) value (first value)))))
  *current-node*)

(defun classes (&rest classes) (apply #'attr :class classes))
(defun dataset (key value)   (attr (concatenate 'string "data-" key) value))
(defun text (s)              (add-child *current-node* s))

(defmacro document (&body body)
  (let ((r (gensym "ROOT")))
    `(let* ((,r (make-instance 'root-node))
           (*current-node* ,r))
       ,@(butlast body)
       (add-child ,r ,(car (last body))))))

;;; Tag macros ---------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %angle (str) (intern (format nil "<~a>" str) *package*))
  (defun %plain (str) (intern (string-upcase str) *package*)))

(defmacro deftag (cls)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
				(unless (find-class ',cls nil) (defclass ,cls (html-element) ())))
     (defmacro ,cls (&rest args)
       (multiple-value-bind (init body) (%parse-tag-args args)
							`(let* ((parent *current-node*)
									(node (make-instance ',',cls)))
							   (setf (attrs node) ',init)
							   (let ((*current-node* node)) 
								 ,@body)
							   (when parent
								 (add-child parent node))
							   node)))
	 (export '(,cls))))

(defmacro deftags (&rest tags) 
  `(progn ,@(mapcar (lambda (tag) `(deftag ,tag)) tags)))

;;; Argument parser -----------------------------------------------------------

(defun %class-list-from-quote (q)
  (cond ((symbolp q) (list (string-downcase (symbol-name q))))
        ((listp q) (mapcan #'%class-list-from-quote q))
        (t (error "class quote must be symbol/list"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-tag-args (args)
    (let ((init nil)
          (classes nil)
          (body nil))
      (loop for rest on args
            for item = (car rest)
            do (cond
                 ;; Keyword attribute
                 ((and (keywordp item) (cdr rest))
                  (push item init)
                  (push (cadr rest) init)
                  (setf rest (cdr rest)))
                 ;; Dot-class syntax
                 ((and (symbolp item)
                       (> (length (symbol-name item)) 1)
                       (char= #\. (char (symbol-name item) 0)))
                  (push (subseq (string-downcase (symbol-name item)) 1) classes))
                 ;; Explicit . syntax
                 ((and (eq item '\.) (cdr rest))
                  (let ((cls (cadr rest)))
                    (setf classes (append (mapcar #'%flatten-value 
                                                 (if (listp cls) cls (list cls))) 
                                         classes))
                    (setf rest (cdr rest))))
                 ;; Quoted class
                 ((and (consp item)
                       (eq (car item) 'quote)
                       (consp (cdr item)))
                  (setf classes (append (%class-list-from-quote (cadr item)) classes)))
                 ;; String content
                 ((stringp item)
                  (push `(text ,item) body))
                 ;; Other forms
                 (t (push item body))))
      (when classes
        (setf init (append init (list :class (nreverse classes)))))
      (values (nreverse init) (nreverse body)))))

(defmacro define-tags (properties &body tags)
  `(progn
     (deftags ,@tags)
	 ,@(when (member :self-closing properties)
		 (mapcar (lambda (tag) 
				   `(defmethod self-closing-p ((n ,tag)) t)) 
				 tags))
	 ,@(when (member :block properties)
		 (mapcar (lambda (tag) 
				   `(defmethod block-element-p ((n ,tag)) t))
				 tags))))

(uiop:define-package :tagflow.html
  (:use :tagflow))

(in-package :tagflow.html)

(define-tags 
 (:block) 
 <html> <head> <body> <script>
 <section> <article> <main> <nav> <footer> <header>
 <h1> <h2> <h3> <h4> <h5> <h6>
 <div> <p> <ul> <ol> <li> <pre>
 <address> <aside> <blockquote> <canvas> <dd> <details> <dl> <dt>
 <fieldset> <figcaption> <figure> <form> <table> <tbody> <tfoot> <thead>
 <tr> <td> <th> <select> <optgroup> <option> <datalist> <menu>)

(define-tags 
 (:inline) 
 <title> 
 <span> <strong> <a> <code>
 <button> <textarea>
 <abbr> <b> <bdi> <bdo> <cite> <del> <dfn> <em> <i> <ins> <kbd> <mark>
 <q> <rp> <rt> <ruby> <s> <samp> <small> <sub> <summary> <sup> <time>
 <u> <var> <label> <legend> <output> <progress> <meter> <audio> <video>
 <iframe> <object> <caption> <colgroup> <keygen> <command>)

(define-tags 
 (:inline :self-closing) 
 <img> <br> <hr> <input>)

(define-tags 
 (:block :self-closing) 
 <link> <meta> <area> <col> <embed> <param> <source> <track> <wbr>)
