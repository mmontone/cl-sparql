(defpackage #:sparql
  (:use #:cl #:parser-combinators)
  (:export #:sparql-query
           #:sparql-query*
           #:sparql
           #:sparql-compile
           #:sparql-eval
           #:enable-uri-syntax
           #:parse-literal
           #:render-literal
           #:make-uri
           #:define-uri-prefix
           #:get-uri-prefix))
