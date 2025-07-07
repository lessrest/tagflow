(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload "tagflow" :silent t)
(ql:quickload "tagflow/tests" :silent t)
