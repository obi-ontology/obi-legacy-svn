(defun create-empty-obi-model ()
  (let ((model (#"createDefaultModel" 'modelfactory)))
    (#"setNsPrefix" model "owl" (uri-full !owl:))
    (#"setNsPrefix" model "xsd" (uri-full !xsd:))
    (#"setNsPrefix" model "rdfs" (uri-full !rdfs:))
    (#"setNsPrefix" model "rdf" (uri-full !rdf:))
    (#"setNsPrefix" model "roproposed" "http://purl.org/obo/owl/OBO_REL#")
    (#"setNsPrefix" model "protege" "http://protege.stanford.edu/plugins/owl/protege#")
    (#"setNsPrefix" model "obo" "http://purl.obolibrary.org/obo/")
    (#"setNsPrefix" model "oboinowl" "http://www.geneontology.org/formats/oboInOwl#")
    (#"setNsPrefix" model "doap" "http://usefulinc.com/ns/doap#")
    model))

(defun write-jena-model (model to)
  (let ((jfile (new 'java.io.file (namestring (translate-logical-pathname to)))))
    (when (not (#"exists" jfile))
      (#"createNewFile" jfile))
    (#"write" model (new 'java.io.fileoutputstream jfile) "RDF/XML-ABBREV")))
    
(defun add-jena-triple (model s property value)
  (let ((subject 
	 (cond ((stringp s)
		(#"createResource" model s))
	       ((uri-p s)
		(#"createResource" model (uri-full s)))
	       ((java-object-p s) s)
	       (t (error "subject: ~a" s))))
	(property (cond ((stringp property)
			 (#"getProperty" model property))
			((uri-p property)
			 (#"getProperty" model (uri-full property)))
			((java-object-p property) property)
			(t (error "property: ~a" s))))
	(value (cond ((and (consp value)
			   (eq (car value) :literal))
		      (make-jena-literal model (second value) (uri-full (third value)))
		      )
		     ((uri-p value)
		      (#"createResource" model (uri-full value)))
		     ((integerp value)
		      (#"createTypedLiteral" model value))
		     ((floatp value)
		      (#"createTypedLiteral" model value))
		     ((java-object-p value) value)
		     (t value))))
    ;(#"addProperty" subject property value)
    (#"add" model subject property value)
    ))

(defun make-jena-literal (model value type)
  (if (equal type (load-time-value (uri-full !rdf:text)))
      (apply #"createLiteral" model (car (all-matches value "(.*)@(.*)" 1 2)))
      (#"createTypedLiteral" model (if (stringp value) value (prin1-to-string value))
			     (new 'jena.datatypes.basedatatype (if (uri-p type) (uri-full type) type)))))