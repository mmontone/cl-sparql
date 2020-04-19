(asdf:defsystem #:cl-sparql
  :description "SPARQL query builder for Common Lisp"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:puri :parser-combinators :cl-ppcre :split-sequence)
  :components ((:file "package")
               (:file "sparql")
               (:file "uri"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-sparql-tests))))
