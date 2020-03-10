(in-package :sparql)

(defun make-uri (prefix uri &rest args)
  (puri:parse-uri
   (concatenate 'string
		(get-uri-prefix prefix)
		(apply #'format nil (cons uri args)))))

(defun clean-literal (literal)
  (remove #\Return literal))

(defmethod render-literal (value)
  (clean-literal (prin1-to-string value)))
(defmethod render-literal ((value integer))
  (format nil "\"~A\"^^<http://www.w3.org/2001/XMLSchema#integer>" value))
(defmethod render-literal ((value string))
  (format nil "~S" value))
(defmethod render-literal ((uri puri:uri))
  (clean-literal (format nil "<~A>" (puri:render-uri uri nil))))
(defmethod render-literal ((x (eql t)))
  "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>")
(defmethod render-literal ((x (eql nil)))
  "\"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>")
(defmethod render-literal ((x symbol))
  (if (keywordp x)
      (format nil "\"~S\"^^<http://common-lisp.net#keyword>" x)
      (format nil "\"~S\"" x)))

(defun parse-typed-literal (literal)
  (cl-ppcre:register-groups-bind (value type)
      ("(.*)\\^\\^(.*)" literal)
    (cond
      ((equalp type "<http://www.w3.org/2001/XMLSchema#integer>")
       (values (parse-integer (read-from-string value))
	       :integer))
      ((equalp type "<http://www.w3.org/2001/XMLSchema#int>")
       (values (parse-integer (read-from-string value))
	       :integer))
      ((equalp type "<http://www.w3.org/2001/XMLSchema#string>")
       (values (read-from-string value)
	       :string))
      ((equalp type "<http://www.w3.org/2001/XMLSchema#boolean>")
       (values (let ((value (read-from-string value)))
		 (if (equalp value "true")
		     t
		     nil))
	       :boolean))
      ((equalp type "<http://common-lisp.net#keyword>")
       (values (read-from-string (read-from-string value))
	       :keyword)))))

(defun parse-boolean-literal (literal)
  (cond
    ((equalp literal "true")
     (values t t))
    ((equalp literal "false")
     (values nil t)))
  (values nil nil))    

(defun parse-literal (literal)
  (let ((literal (clean-literal literal)))
    (let* ((first (char literal 0)))
      (multiple-value-bind (value type)
	  (parse-typed-literal literal)
	(when type
	  (return-from parse-literal value)))
      (cond
	((equalp first #\")
	 (subseq literal 1 (1- (length literal))))
	((equalp first #\<)
	 (puri:parse-uri (subseq literal 1 (1- (length literal)))))
	((parse-integer literal :junk-allowed t)
	 (parse-integer literal :junk-allowed t))
	((multiple-value-bind (value parsed-p)
	     (parse-boolean-literal literal)
	   (declare (ignore value))
	   parsed-p)
	 (parse-boolean-literal literal))	   
	(t literal)))))

(defvar *uri-prefixes* (make-hash-table :test #'equalp))

(defmacro define-uri-prefix (uri-prefix value &optional documentation)
  (declare (ignore documentation))
  `(setf (gethash (symbol-name ',uri-prefix) *uri-prefixes*) ,value))

(define-uri-prefix rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define-uri-prefix xsd "http://www.w3.org/2001/XMLSchema#")
(define-uri-prefix ex "http://www.franz.com/things#")  
(define-uri-prefix rdfs "http://www.w3.org/2000/01/rdf-schema#")  
(define-uri-prefix fn "http://www.w3.org/2005/xpath-functions#") 
(define-uri-prefix err "http://www.w3.org/2005/xqt-errors#")  
(define-uri-prefix owl "http://www.w3.org/2002/07/owl#")  
(define-uri-prefix xs "http://www.w3.org/2001/XMLSchema#")

(defun display-uri-prefixes ()
  (loop for uri-prefix being the hash-keys of *uri-prefixes*
       using (hash-value value)
       do (format t "~A => ~A~%" uri-prefix value)))

(defun get-uri-prefix (uri-prefix)
  (gethash (or (and (stringp uri-prefix) uri-prefix)
	       (and (symbolp uri-prefix)
		    (symbol-name uri-prefix))) *uri-prefixes*))

(defun parse-uri (uri)
  (let ((split-uri
	 (split-sequence:split-sequence #\: uri)))
    (if (and (first split-uri)
	     (get-uri-prefix (first split-uri)))
	(puri:parse-uri
	 (concatenate 'string
		      (get-uri-prefix (first split-uri))
		      (apply #'concatenate 'string (rest split-uri))))
	(puri:parse-uri uri))))

(defun uri-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((chars
	 (loop for char = (read-char stream)
	    while (not (equalp char #\>))
	    collect char)))
    (parse-uri (coerce chars 'string))))

(defun enable-uri-syntax ()
  (set-dispatch-macro-character
   #\# #\< #'uri-reader))

;; Example
;; #<http://www.google.com>
;; #<rdf:type>
