(defsystem "tagflow"
  :description "A HTML DSL"
  :author "Mikael Brockman"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "tagflow"))
  :in-order-to ((test-op (test-op "tagflow/tests"))))

(defsystem "tagflow/tests"
  :description "Test suite for tagflow"
  :author "Your Name"
  :license "MIT"
  :depends-on (#:tagflow #:fiveam)
  :components ((:file "tests"))
  :perform (test-op (op c) (symbol-call :fiveam :run! :tagflow-tests)))
