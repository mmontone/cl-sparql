# CL-SPARQL

SPARQL query builder for Common Lisp.

## Syntax

```lisp
(sparql (:select * :where (?x ?y ?z)))
```

```lisp
 (sparql (:select *
			:where (?x ?y ?z)
			:limit 20
			:offset 10))
```

### Subqueries

```lisp
(sparql (:select *
                        :where (?x ?y ?z)
                        (:select *
                                 :where (?x ?y ?z)
                                 :limit 20
                                 :offset 10)
                        :limit 20
                        :offset 10))
``` 

## URI syntax

Enable special URI syntax with `(sparql:enable-uri-syntax)`.

After that, you can write uris like:

`#<rdf:type>`

and you get the expanded uri:

`#<PURI:URI http://www.w3.org/1999/02/22-rdf-syntax-ns#type>`.

Use `define-uri-prefix` to define new uri prefixes expansions, like:

```lisp
(define-uri-prefix rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define-uri-prefix xsd "http://www.w3.org/2001/XMLSchema#")
(define-uri-prefix ex "http://www.franz.com/things#")  
(define-uri-prefix rdfs "http://www.w3.org/2000/01/rdf-schema#")  
(define-uri-prefix fn "http://www.w3.org/2005/xpath-functions#") 
(define-uri-prefix err "http://www.w3.org/2005/xqt-errors#")  
(define-uri-prefix owl "http://www.w3.org/2002/07/owl#")  
(define-uri-prefix xs "http://www.w3.org/2001/XMLSchema#")
```

Those are already defined.


## Implementation technique

I think the embedded domain specific language technique used is interesting. `parser-combinators` library is used, but, instead of using for parsing strings, it is used for parsing already parsed Lisp lists.

## License

MIT

