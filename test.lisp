(defpackage :cl-sparql-test
  (:use :cl :sparql :fiveam)
  (:export #:run-tests))

(in-package :cl-sparql-test)

#.(sparql:enable-uri-syntax)

(defun run-tests ()
  (run! 'cl-sparql-test))

(def-suite cl-sparql-test)

(in-suite cl-sparql-test)

(test parse-literal-test
  (is (equalp (parse-literal "2") 2))
  (is (equalp (parse-literal "\"2\"^^<http://www.w3.org/2001/XMLSchema#integer>") 2))
  (is (equalp (parse-literal "\"hello\"") "hello"))
  (is (equalp (parse-literal "\"hello\"^^<http://www.w3.org/2001/XMLSchema#string>") "hello"))
  (is (puri:uri= (parse-literal "<http://bonanza.cl.no>") (puri:parse-uri "http://bonanza.cl.no")))
  (is (equalp (parse-literal "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>") t))
  (is (equalp (parse-literal "\"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>") nil))

  (signals error
    (parse-literal "\"hello\"^^<http://www.w3.org/2001/XMLSchema#integer>")))

(defun remove-spaces (string)
  (with-input-from-string (stream string)
    (loop
       with new-string
       for char = (read-char stream nil nil)
       while char
       when (not (equalp char #\ ))
     do (push char new-string)
     finally (return (coerce (nreverse new-string) 'string)))))
       
(defun sparql-str-equalp (str1 str2)
  (string-equal (remove-spaces str1)
		(remove-spaces str2)))

(test sparql-syntax-test

  ;; Basic tests
  
  (is (sparql-str-equalp
       (sparql (:select * :where (?x ?y ?z)))
       "SELECT * WHERE { ?X ?Y ?Z}"))
  (is (sparql-str-equalp
       (sparql (:select ?x :where (?x ?y ?z)))
       "SELECT ?x WHERE { ?X ?Y ?Z}"))

  ;; Literals
  (is (sparql-str-equalp
       (sparql (:select * :where (?x ?y ?z)
			(?x #<rdf:type> "MyType")))
      "SELECT * WHERE { ?X ?Y ?Z . ?X <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> \"MyType\"}"))

  ;; Select options
  (is (sparql-str-equalp
       (sparql (:select *
			:where (?x ?y ?z)
			:order-by ?z))
       "SELECT * WHERE { ?X ?Y ?Z} ORDER BY ASC(?Z)"))

  (is (sparql-str-equalp
       (sparql (:select *
			:where (?x ?y ?z)
			:limit 20
			:offset 10))
       "SELECT * WHERE { ?X ?Y ?Z} LIMIT 20 OFFSET 10"))

  ;; Subqueries
  (is (sparql-str-equalp
       (sparql (:select *
			:where (?x ?y ?z)
			(:select *
				 :where (?x ?y ?z)
				 :limit 20
				 :offset 10)
			:limit 20
			:offset 10))
       "SELECT * WHERE { ?X ?Y ?Z . { SELECT * WHERE { ?X ?Y ?Z} LIMIT 20 OFFSET 10 } } LIMIT 20 OFFSET 10"))
  
  ;; Variable capture
  
  (is (sparql-str-equalp
       (let ((value nil))
	 (sparql (:select * :where (?x ?y value))))
       "SELECT * WHERE { ?X ?Y \"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>}"))
  (is (sparql-str-equalp
       (let ((value "hello"))
	 (sparql (:select * :where (?x ?y value))))
       "SELECT * WHERE { ?X ?Y \"hello\"}"))

  ;; SPARQL evaluation
  (is (sparql-str-equalp
       (let ((order-by (list :order-by '?timestamp))
	     (value "Foo"))
	 (sparql-eval `(:select * :where
				(?x #<rdfs:label> ,value)
				,@order-by)))
       "SELECT * WHERE { ?X <http://www.w3.org/2000/01/rdf-schema#label> \"Foo\"} ORDER BY ASC(?TIMESTAMP)")))
