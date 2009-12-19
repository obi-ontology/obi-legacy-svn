(defparameter *obi-prefixes*
  '(("xsd" "http://www.w3.org/2001/XMLSchema#")
    ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
    ("owl" "http://www.w3.org/2002/07/owl#")
    ("daml" "http://www.daml.org/2001/03/daml+oil#")
    ("dcterms" "http://purl.org/dc/terms/")
    ("dc" "http://purl.org/dc/elements/1.1/")
    ("protege" "http://protege.stanford.edu/plugins/owl/protege#")
    ("protege-dc" "http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl#")
    ("oboInOwl" "http://www.geneontology.org/formats/oboInOwl#")
    ("bfo" "http://www.ifomis.org/bfo/1.1#")
    ("robfo" "http://purl.org/obo/owl/ro_bfo_bridge/1.1#")
    ("snap" "http://www.ifomis.org/bfo/1.1/snap#")
    ("span" "http://www.ifomis.org/bfo/1.1/span#")
    ("ro" "http://www.obofoundry.org/ro/ro.owl#")
    ("rotoo" "http://purl.org/obo/owl/ro#")
    ("pato" "http://purl.org/obo/owl/PATO#")
    ("cell" "http://purl.org/obo/owl/CL#")
    ("chebi" "http://purl.org/obo/owl/CHEBI#")
    ("envo""http://purl.org/obo/owl/ENVO#")
    ("ncbitax""http://purl.org/obo/owl/NCBITaxon#")
    ("obi" "http://purl.obolibrary.org/obo/")
    ("caro" "http://purl.org/obo/owl/CARO#")
    ("pro" "http://purl.org/obo/owl/PRO#")
    ("so" "http://purl.org/obo/owl/SO#")
    ("go" "http://purl.org/obo/owl/GO#")
    ("obi_denrie" "http://purl.obolibrary.org/obo/obi/DigitalEntityPlus.owl#")
    ("obi_biomat" "http://purl.obolibrary.org/obo/obi/Biomaterial.owl#")
    ("obi_extd" "http://purl.obolibrary.org/obo/obi/externalDerived.owl#")
    ("obi_rel" "http://purl.obolibrary.org/obo/obi/Relations.owl#")
    ("obi_plan" "http://purl.obolibrary.org/obo/obi/PlanAndPlannedProcess.owl#")
    ("obi_role" "http://purl.obolibrary.org/obo/obi/Role.owl#")
    ("obi_instr" "http://purl.obolibrary.org/obo/obi/InstrumentAndPart.owl#")
    ("obi_func" "http://purl.obolibrary.org/obo/obi/OBI-Function.owl#")
    ("obi_annot" "http://purl.obolibrary.org/obo/obi/AnnotationProperty.owl#")
    ("obi_ext" "http://purl.obolibrary.org/obo/obi/external.owl#")
    ("obi_quality" "http://purl.obolibrary.org/obo/obi/Quality.owl#")
    ("obi_owlfull" "http://purl.obolibrary.org/obo/obi/obi-owl-full.owl#")))

(defparameter *external-derived-header*
  "<?xml version=\"1.0\"?>
<rdf:RDF
  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
  xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
  xmlns:owl=\"http://www.w3.org/2002/07/owl#\"
  xml:base=\"http://purl.obolibrary.org/obo/obi/externalDerived.owl#\">
  <owl:Ontology rdf:about=\"\">
    <owl:versionInfo rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\"
    >$Revision: 80 $</owl:versionInfo>
  </owl:Ontology>
")

(defun parse-templates (templates-path)
  (with-open-file (f templates-path)
    (let* ((params 
	    (loop for line = (read-line f nil :eof)
	       until (or (eq line :eof) (search "==" line))
	       unless (or (char= (char line 0) #\#) (equal line ""))
	       append (all-matches line "(.*?):(.*)" 1 2) into them
	       when (eql #\= (peek-char t f)) do (return them)))
	   (prefixes (with-output-to-string (s)
		       (loop for (param value) in params
			    when (equalp param "prefix")
			    do (format s "prefix ~a~%" value))))
	   (templates nil))
      (flet ((ont+query (lines)
	       (setq lines (remove #\# (remove "" (reverse lines) :test 'equal) :key (lambda(el) (char el 0))))
	       (let ((ont (caar (all-matches (car lines) "==\\s+(.*?)\\s+==" 1)))
		     (query
		      (replace-all
		       (concatenate 'string prefixes
				    (join-with-char (rest lines) #\linefeed))
		         "alias:([a-zA-z0-9._-]*)" (lambda(alias)
					     (#"replaceFirst"
					      (second
					       (find alias params :test (lambda(what el)
									  (declare (ignore what))
									  (and (equal (car el) "alias")
									       (eql 0 (search alias (second el)))))))
					       ".*=(.*)" "$1"))
			 1)))
		 (when (search "_GRAPH_GOES_HERE_" query)
		   (setq graph-was-seen t))
		 (if (assoc ont templates :test 'equalp)
		     (pushnew query (cadr (assoc ont templates :test 'equalp)) :test 'equal)
		     (push (list ont (list query)) templates))
		 )))
	(loop for line = (read-line f nil :eof)
	   with lines
	   until (eq line :eof)
	   do
	     (push line lines)
	     (when (or (eql #\= (peek-char t f nil)) (null (peek-char t f nil)))
	       (ont+query lines) (setq lines nil))
	   finally (when lines (ont+query lines))))
      (assert (or (and graph-was-seen (second (assoc "default graph_base" params :test 'equal)))
		  (not graph-was-seen)) ()
		  "_GRAPH_GOES_HERE_ used but 'default graph_base' not asserted in file")
      (values params templates))))

(defun combine-template-query-results (results output-path)
  (with-open-file (f output-path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write-string *external-derived-header* f)
    (loop for rdf in results
       for lines = (split-at-char rdf #\linefeed)
       do
       (loop for line in (butlast (cddr lines))
	  do (format f "~a~%" line)))
    (format f "<owl:AnnotationProperty rdf:about=\"http://purl.obolibrary.org/obo/IAO_0000115\"/><owl:AnnotationProperty rdf:about=\"http://purl.obolibrary.org/obo/IAO_0000111\"/><owl:AnnotationProperty rdf:about=\"http://purl.obolibrary.org/obo/IAO_0000412\"/>")
    (format f "</rdf:RDF>~%")))

(defun clean-rdf (path prefixmapping)
  (let* ((file (maybe-url-filename path))
	 (model (#"createOntologyModel" 'com.hp.hpl.jena.rdf.model.ModelFactory (get-java-field 'OntModelSpec "OWL_MEM"))))
    (let ((base "http://purl.obolibrary.org/obo/obi/externalDerived.owl"))
      (#"read" model (new 'io.bufferedinputstream (#"getInputStream" (#"openConnection" (new 'java.net.url file)))) base)
      (loop for (prefix namespace) in prefixmapping
	 do (#"setNsPrefix" (#"getPrefixMapping" (#"getGraph" model)) prefix namespace))
      (let ((writer (#"getWriter" model "RDF/XML-ABBREV")))
	(#"setProperty" writer "showXmlDeclaration" "true")
	(#"setProperty" writer "xmlbase" base)
	(#"setProperty" writer "relativeURIs" "")
	(#"write" writer model (new '|FileOutputStream| path) base)
	))))

(defparameter *smart-leftquote-pattern* (coerce (list #\. (code-char 196) (code-char 242)) 'string))
(defparameter *smart-rightquote-pattern* (coerce (list #\. (code-char 196) (code-char 244)) 'string))

(defun xml-encode-unicode-high (string)
  (let ((matcher (#"matcher" (load-time-value (#"compile" 'java.util.regex.pattern "([\\u0080-\\uFFFF])")) string))
	(sb (new 'stringbuffer)))
    (loop while (#"find" matcher) 
	  do
	  (#"appendReplacement" matcher sb (format nil "&#x~4,'0x;" (#"codePointAt" (#0"group" matcher 0) 0))))
    (#"appendTail" matcher sb)
    (#"toString" sb)))

(defun create-external-derived (&key
				(kb (load-kb-jena "obi:branches;external.owl"))
				(templates-path "obi:lisp;external-templates.txt")
				(output-path (merge-pathnames
					      "externalDerived.owl"
					      (truename "obi:branches;")))

				(endpoint nil)
				(debug nil))
  (let ((*sparql-always-trace* (or *sparql-always-trace* debug)))
    (declare (special *sparql-always-trace*))
    (let ((terms 
	   (sparql '(:select (?term ?where ?parent) () 
					;(?term !rdf:type !owl:Class)
		     (?term !obi:IAO_0000412 ?where)
		     (:union ((?term !rdfs:subClassOf ?parent)) ((?term !rdf:type ?parent) (:filter (not (equal ?parent !owl:Class)))))
		     )
		   :use-reasoner :none ;; turn the reasoner off, so that we don't get the obi superclasses
		   :kb kb :trace t))
	  (classes 
      	   (sparql '(:select (?term) () 
		     (?term !rdf:type !owl:Class))
		   :use-reasoner :none ;; turn the reasoner off, so that we don't get the obi superclasses
		   :kb kb :trace t))
	  (instances 
	   (sparql '(:select (?term ?type) () 
		     (?term !rdf:type ?type)
		     (:filter (and (not (regex (str ?type) "^http://www.w3.org/2002/07/owl#")))))
		   :use-reasoner :none ;; turn the reasoner off, so that we don't get the obi superclasses
		   :kb kb :trace t)))
      (format t "There are ~a external terms - ~a classes and ~a instances~%" (length terms) (- (length terms) (length instances)) (length instances))
      (multiple-value-bind (params templates) (parse-templates templates-path)
	(let ((endpoint (or (second (assoc "default endpoint" params :test 'equal)) endpoint))
	      (graph_base (second (assoc "default graph_base" params :test 'equal))))
	  (assert endpoint () "What endpoint should I use?")
	  (format t "Using endpoint: ~a~%" endpoint)
	  (let ((rdfs 
		 (append
		  (loop for query in (cadr (assoc "Once Only" templates :test 'equalp))
		     collect (get-url endpoint :post `(("query" ,query)) :persist nil :dont-cache t :force-refetch t))
		  (loop for (term where) in terms
		     for graph = (#"replaceAll" (uri-full where) ".*[/#](.*?)(\.owl){0,1}" "$1")
		     ;; when debug do (print-db graph)
		     append
		     (loop for (ont-pattern queries) in templates
			when (#"matches" (uri-full where)  (format nil "(?i)~a" ont-pattern))
			append
			(loop for query in queries
			   for filled-query = 
			   (#"replaceAll" (#"replaceAll" query "_ID_GOES_HERE_" (format nil "<~a>" (uri-full term))) "_GRAPH_GOES_HERE_" (format nil "<~a~a>" graph_base (#"replaceAll" (string-upcase graph) ".OWL$" "")))
			     do (when debug
				     (progn
				       (print-db (uri-full term))
				       (print-db filled-query)
				       (print (get-url endpoint :post `(("query" ,filled-query)) :persist nil :dont-cache t :force-refetch t))))

			   ;; FIXME - Horrible workaround to compensate for abcl not understanding unicode and VO using smart quotes
			   ;; Explain to me why the get URL produces different strings on a mac compared to a linux box. Byte order?
			   collect (xml-encode-unicode-high (setq foo (#"replaceAll" (#"replaceAll" (setq bar (get-url endpoint :post `(("query" ,filled-query)) :persist nil :dont-cache t :force-refetch t)) *smart-leftquote-pattern* "&#8216;") *smart-rightquote-pattern* "&#8217;")))))))))
	    (let ((basic-info
		   (with-output-to-string (s)
		     (write-string "<?xml version=\"1.0\" encoding=\"utf-8\" ?>" s) (terpri s)
		     (write-string "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\">" s) (terpri s)
		     (loop for (class) in classes
			do (format s "<owl:Class rdf:about=~s></owl:Class>~%"
				   (uri-full class) ))
		     (loop for (instance type) in instances
			do (format s "<rdf:Description rdf:about=~s><rdf:type rdf:resource=~s/></rdf:Description>~%"
				   (uri-full instance) (uri-full type) )
			  (format t "<rdf:Description rdf:about=~s><rdf:type rdf:resource=~s/></rdf:Description>~%"
				   (uri-full instance) (uri-full type) ))
		     (write-string "</rdf:RDF>" s)
		     )))
	      (combine-template-query-results (cons basic-info rdfs) output-path))
	    (clean-rdf (namestring (truename output-path)) *obi-prefixes*)
	    nil
	    ))))))
