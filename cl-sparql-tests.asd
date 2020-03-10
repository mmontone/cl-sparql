(defsystem :cl-sparql-tests
  :author  "Mariano Montone"
  :license "MIT"
  :description "CL-SPARQL tests"
  :version "0.0.1"
  :depends-on (:cl-sparql :fiveam)
  :serial t
  :components ((:file "test"))
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :cl-sparql-test :run-tests)))
