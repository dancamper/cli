(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :with-user-abort :flexi-streams :fxml) :silent t))

(defpackage :xml2ecl
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :xml2ecl)

;;;; Configuration -----------------------------------------------
(defparameter *ecl-keywords* '("__alias__" "__common__" "__compound__" "__compressed__" "__debug__"
                               "__ecl_legacy_mode__" "__ecl_version__" "__ecl_version_major__"
                               "__ecl_version_minor__" "__ecl_version_subminor__" "__grouped__"
                               "__line__" "__nameof__" "__nostreaming__" "__option__" "__os__"
                               "__owned__" "__platform__" "__sequence__" "__set_debug_option__"
                               "__stand_alone__" "__target_platform__" "_array_" "_empty_"
                               "_linkcounted_" "abs" "acos" "action" "after" "aggregate" "algorithm"
                               "all" "allnodes" "and" "any" "apply" "as" "ascii" "asin" "assert"
                               "asstring" "atan" "atan2" "atmost" "ave" "backup" "before" "beginc++"
                               "best" "between" "big_endian" "bitfield" "bitmap" "blob" "bloom"
                               "bnot" "boolean" "build" "buildindex" "c++" "cardinality" "case"
                               "catch" "checkpoint" "choose" "choosen:all" "choosen" "choosesets"
                               "cluster" "clustersize" "cogroup" "combine" "compressed" "const"
                               "correlation" "cos" "cosh" "count" "counter" "covariance" "critical"
                               "cron" "csv" "data" "dataset" "decimal" "dedup" "default" "define"
                               "denormalize" "deprecated" "desc" "descend" "dictionary" "distribute"
                               "distributed" "distribution" "div" "dynamic" "ebcdic" "eclcrc" "elif"
                               "else" "elseif" "elsif" "embed" "embedded" "encoding" "encrypt"
                               "encrypted" "end" "endc++" "endembed" "endmacro" "enth" "enum"
                               "error" "escape" "evaluate" "event" "eventextra" "eventname" "except"
                               "exclusive" "exists" "exp" "expire" "export" "extend" "fail"
                               "failcode" "failmessage" "failure" "false" "feature" "fetch" "few"
                               "fileposition" "filtered" "first" "fixed" "flat" "format" "forward"
                               "from" "fromjson" "fromunicode" "fromxml" "full" "function"
                               "functionmacro" "getenv" "getsecret" "global" "graph" "group"
                               "groupby" "grouped" "guard" "hash" "hash32" "hash64" "hashcrc"
                               "hashmd5" "having" "heading" "hint" "httpcall" "httpheader" "if"
                               "ifblock" "iff" "ignore" "import" "in" "independent" "index" "inner"
                               "integer" "interface" "internal" "intformat" "isnull" "isvalid"
                               "iterate" "join" "joined" "json" "keep" "keydiff" "keyed" "keypatch"
                               "keyunicode" "label" "labeled" "labelled" "last" "left" "length"
                               "library" "likely" "limit" "linkcounted" "literal" "little_endian"
                               "ln" "loadxml" "local" "locale" "localfileposition" "log"
                               "logicalfilename" "lookup" "loop" "lzw" "macro" "many" "map"
                               "matched" "matchlength" "matchposition" "matchrow" "matchtext"
                               "matchunicode" "matchutf8" "max" "maxcount" "maxlength" "maxsize"
                               "merge" "mergejoin" "min" "module" "mofn" "multiple" "named"
                               "namespace" "noboundcheck" "nocase" "nocombine" "noconst" "nofold"
                               "nohoist" "nolocal" "nonempty" "nooverwrite" "normalize" "noroot"
                               "noscan" "nosort" "not" "nothor" "notify" "notrim" "noxpath"
                               "omitted" "once" "onfail" "onfail" "only" "onwarning" "opt" "or"
                               "ordered" "out" "outer" "output" "overwrite" "packed" "parallel"
                               "parse" "partition" "pattern" "penalty" "persist" "pipe" "plane"
                               "power" "prefetch" "preload" "priority" "private" "probability"
                               "process" "project" "proxyaddress" "pull" "pulled" "qstring"
                               "quantile" "queue" "quote" "random" "range" "rank" "ranked" "real"
                               "realformat" "record" "recordof" "recordset" "recovery" "refresh"
                               "regexfind" "regexfindset" "regexreplace" "regroup" "rejected"
                               "remote" "repeat" "response" "restricted" "retry" "return" "right"
                               "rollup" "round" "roundup" "row" "rowdiff" "rows" "rowset" "rule"
                               "sample" "scan" "score" "section" "self" "separator" "sequential"
                               "service" "set of" "set" "shared" "sin" "single" "sinh" "size_t"
                               "sizeof" "skew" "skip" "smart" "soapaction" "soapcall" "sort"
                               "sorted" "sql" "sqrt" "stable" "stepped" "stored" "streamed"
                               "string" "subsort" "success" "sum" "table" "tan" "tanh" "terminator"
                               "then" "thisnode" "thor" "threshold" "timelimit" "timeout" "tojson"
                               "token" "topn" "tounicode" "toxml" "trace" "transfer" "transform"
                               "trim" "true" "truncate" "type" "typeof" "udecimal" "ungroup"
                               "unicodeorder" "unlikely" "unordered" "unsigned" "unsorted"
                               "unstable" "update" "use" "utf8" "validate" "variance" "varstring"
                               "varunicode" "virtual" "volatile" "wait" "warning" "when" "which"
                               "whitespace" "whole" "width" "wild" "within" "workunit" "xml"
                               "xmldecode" "xmldefault" "xmlencode" "xmlns" "xmlproject" "xmltext"
                               "xmlunicode" "xpath"))

(defvar *layout-names* nil
  "Used while ECL record definitions are being emitted.  Tracks the names
of the record definitions created, so that subsequent creations don't reuse
previously-defined names.")

(defparameter *ecl-string-type* "UTF8"
  "The ECL data type to be used for XML string types.  Can be overridden
with an option.")

(defparameter *wrapper-xml-tag* (the string "wrapper"))

(defparameter *inner-text-tag* (the string "_inner_value")
  "The ECL field name to use for value found within an XML tag when that
tag has attributes, or in cases where a simple tag is repeated within
an XML scope.")

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(define-condition missing-file (user-error)
  ((path :initarg :path))
  (:report
   (lambda (c s)
     (format s "missing file '~A'" (slot-value c 'path)))))

(define-condition unknown-input (user-error)
  ((thing :initarg :thing))
  (:report
   (lambda (c s)
     (format s "unknown input '~A'" (slot-value c 'thing)))))

;;;; Functionality -----------------------------------------------
(defclass base-object ()
  ((visit-count :accessor visit-count :initform 0 :type fixnum)
   (max-visit-count :accessor max-visit-count :initform 0 :type fixnum)))

(defclass xml-object (base-object)
  ((children :accessor children :initform (make-hash-table :test 'equalp :size 25))
   (attrs :accessor attrs :initform (make-hash-table :test 'equalp :size 10))))

(defmethod reset-visits ((obj t))
  )

(defmethod reset-visits ((obj base-object))
  (setf (max-visit-count obj) (max (the fixnum (visit-count obj))
                                   (the fixnum (max-visit-count obj)))
        (visit-count obj) 0))

(defmethod reset-children-visits ((obj t))
  )

(defmethod reset-children-visits ((obj xml-object))
  (loop for child being the hash-values of (children obj)
        do (reset-visits child)))

(defmethod only-inner-text-p ((obj t))
  nil)

(defmethod only-inner-text-p ((obj xml-object))
  (and (zerop (hash-table-count (attrs obj)))
       (= (hash-table-count (children obj)) 1)
       (gethash *inner-text-tag* (children obj))))

(defmethod single-inner-text-p ((obj t))
  nil)

(defmethod single-inner-text-p ((obj xml-object))
  (and (= (the fixnum (max-visit-count obj)) 1)
       (only-inner-text-p obj)))

;;;

(defun is-ecl-keyword-p (name)
  "Test if NAME (which should be a lowercase string) is an ECL keyword."
  (declare (string name))
  (member name *ecl-keywords* :test 'equalp))

(defun remove-illegal-chars (name &key (replacement-char #\_) (keep-char-list '()))
  "Return a copy of NAME with characters illegal for ECL attribute names
substituted with a replacment character, then reducing runs of those
replacement characters down to a single occurrence."
  (declare (string name))
  (let* ((keep-chars (reduce 'cons keep-char-list
                             :initial-value (list #\_ replacement-char)
                             :from-end t))
         (initial (substitute-if replacement-char
                                 (lambda (c) (not (or (alphanumericp c) (member c keep-chars))))
                                 name)))
    (with-output-to-string (s)
      (loop for c across initial
            with skip = nil
            do (progn
                 (unless (and (eql c replacement-char) skip)
                   (format s "~A" c))
                 (setf skip (eql c replacement-char)))))))

;;;

(defun apply-prefix (name prefix-char)
  "Conditionally append  prefix PREFIX-CHAR to NAME."
  (declare (simple-string name))
  (format nil "~A~A~A"
          prefix-char
          (if (char= (elt name 0) #\_) "" "_")
          name))

(defun legal-layout-subname (name)
  "Return a copy of NAME that can be used within a RECORD name."
  (declare (string name))
  (let ((initial (string-upcase (remove-illegal-chars name))))
    (if (not (alpha-char-p (elt initial 0)))
        (apply-prefix initial "F")
        initial)))

(defun register-layout-subname (name)
  "Push layout subname NAME to a special variable list so we can track usage."
  (declare (string name))
  (let ((legal-name (legal-layout-subname name)))
    (push legal-name *layout-names*)))

;;;

(declaim (inline common-base-type))
(defun common-base-type (new-type old-type)
  "Given two internal data types, return an internal type that can encompass both.
Neither of the arguments can be null."
  (declare (symbol new-type old-type)
           (dynamic-extent new-type))
  (flet ((is-arg-p (x)
           (or (eql x new-type) (eql x old-type))))
    (cond ((eql new-type old-type)
           new-type)
          ((is-arg-p 'default-string)
           'default-string)
          ((is-arg-p 'string)
           'string)
          ((and (is-arg-p 'neg-number) (is-arg-p 'pos-number))
           'neg-number)
          ((and (or (is-arg-p 'neg-number) (is-arg-p 'pos-number))
                (is-arg-p 'float))
           'float)
          (t
           'string))))

(defun reduce-base-type (types)
  "Apply `common-base-type' to all elements in a list, reducing to a single base type."
  (reduce #'common-base-type types))

(defun base-type (value)
  "Determine the basic internal data type of VALUE."
  (if (not value)
      'default-string
      (let ((value-str (if (stringp value) value (format nil "~A" value))))
        (declare (simple-string value-str))
        (cond ((string= value-str "")
               'default-string)
              ((or (string-equal value-str "true") (string-equal value-str "false"))
               'boolean)
              (t
               (loop named char-walker
                     for c across value-str
                     for pos fixnum from 0
                     with decimal-char-found-p = nil
                     with found-type symbol = nil
                     do (progn
                          (cond ((and (eql c #\-) (zerop pos))
                                 (setf found-type 'neg-number))
                                ((and (eql c #\+) (zerop pos))
                                 (setf found-type 'pos-number))
                                ((digit-char-p c)
                                 (setf found-type (if (plusp pos)
                                                      (common-base-type 'pos-number found-type)
                                                      'pos-number)))
                                ((and (eql c #\.) (not decimal-char-found-p))
                                 (setf decimal-char-found-p t
                                       found-type (if (plusp pos)
                                                      (common-base-type 'float found-type)
                                                      'float)))
                                (t
                                 (return-from char-walker 'default-string))))
                     finally (progn
                               (when (and (= pos 1)
                                          (or (eql found-type 'neg-number)
                                              (eql found-type 'float)))
                                 (setf found-type 'string))
                               (return-from char-walker found-type))))))))

;;;

(defun first-hash-table-kv (hash-table)
  (loop for k being the hash-keys of hash-table
          using (hash-value v)
        do (return (values k v))))

(defun first-hash-table-key (hash-table)
  (first-hash-table-kv hash-table))

(defun first-hash-table-value (hash-table)
  (multiple-value-bind (k v) (first-hash-table-kv hash-table)
    (declare (ignore k))
    v))

;;;

(defmacro reuse-object (place classname)
  "Return object found in PLACE if it is an instance of CLASSNAME, or create a
new instance of CLASSNAME in place and return that."
  `(progn
     (cond ((or (null ,place) (not ,place) (eql ,place 'null-value))
            (setf ,place (make-instance ,classname)))
           ((and (consp ,place) (eql (car ,place) 'null-value))
            (setf ,place (make-instance ,classname)))
           ((not (typep ,place ,classname))
            (error "xml2ecl: Mismatching object types; expected ~A but found ~A"
                   ,classname
                   (type-of ,place))))
     (incf (the fixnum (visit-count ,place)))
     ,place))

(defmacro parse-simple (place value)
  "Pushes the base type of VALUE onto the sequence PLACE."
  `(unless (or (typep ,place 'xml-object)
               (equalp ,place '(default-string)))
     (pushnew (the symbol (base-type ,value)) ,place) :test #'eql))

(defmacro parse-complex (place classname source)
  "Reuse object in PLACE if possible, or create a new instance of CLASSNAME,
then kick off a new depth of parsing with the result."
  `(progn
     (reuse-object ,place ,classname)
     (parse-attrs ,place ,source)
     (parse-obj ,place ,source)))

;;;

(defun as-layout-name (name)
  "Construct a unique string that is a suitable ECL RECORD attribute, based on NAME."
  (declare (string name))
  (let* ((legal-name (legal-layout-subname name))
         (found-count (count-if #'(lambda (x) (equalp x legal-name)) *layout-names*))
         (interstitial (if (< found-count 2) "" (format nil "_~3,'0D" found-count))))
    (format nil "~A~A_LAYOUT" legal-name interstitial)))

(defun as-ecl-field-name (name)
  "Return a copy of NAME that is suitable to be used as an ECL attribute."
  (declare (simple-string name))
  (let ((lowername (string-downcase name)))
    (declare (type simple-string lowername))
    (if (string-equal lowername *inner-text-tag*)
        lowername
        (let ((no-dashes (remove-illegal-chars lowername)))
          (if (or (not (alpha-char-p (elt no-dashes 0)))
                  (is-ecl-keyword-p no-dashes))
              (apply-prefix no-dashes "f")
              no-dashes)))))

(defun as-ecl-xpath (name attributep)
  "Construct an ECL XPATH directive for NAME (typically an as-is XML tag)."
  (declare (string name))
  (if (string-equal name *inner-text-tag*)
      "{XPATH('')}"
      (let ((cleaned-name (remove-illegal-chars name :replacement-char #\* :keep-char-list '(#\-)))
            (attr-prefix (if attributep "@" "")))
        (format nil "{XPATH('~A~A')}" attr-prefix cleaned-name))))

(defun as-dataset-type (name)
  "Construct an ECL DATASET datatype, given NAME."
  (declare (string name))
  (format nil "DATASET(~A)" (as-layout-name name)))

(defun as-ecl-type (value-type)
  "Given a symbol representing an internal data type, return the corresponding ECL data type."
  (if (consp value-type)
      (as-ecl-type (reduce-base-type value-type))
      (case value-type
        (boolean "BOOLEAN")
        (null-value "STRING")
        (string "STRING")
        (default-string *ecl-string-type*)
        (pos-number "UNSIGNED")
        (neg-number "INTEGER")
        (float "REAL"))))

(defun as-value-comment (value-type)
  "If VALUE-TYPE is a list of more than one base type, return a string that serves
as an ECL comment describing those types."
  (when (and (consp value-type)
             (or (and (= (length value-type) 1)
                      (eql (car value-type) 'null-value))
                 (and (> (length value-type) 1)
                      (member (as-ecl-type value-type) '(*ecl-string-type* "STRING") :test #'string-equal))))
    (flet ((desc (v)
             (case v
               (null-value "null")
               (default-string "string")
               (pos-number "unsigned integer")
               (neg-number "signed integer")
               (t (format nil "~(~A~)" v)))))
      (format nil "// ~{~A~^, ~}" (mapcar #'desc value-type)))))

;;;

(defgeneric as-ecl-field-def (value-obj name attributep)
  (:documentation "Create an ECL field definition from an object or array class."))

(defmethod as-ecl-field-def ((value-obj t) name attributep)
  (declare (string name)
           (boolean attributep))
  (let ((ecl-type (as-ecl-type value-obj))
        (xpath (as-ecl-xpath name attributep))
        (comment (as-value-comment value-obj)))
    (with-output-to-string (s)
      (format s "~4T~A ~A ~A;" ecl-type (as-ecl-field-name name) xpath)
      (when comment
        (format s " ~A" comment))
      (format s "~%"))))

(defmethod as-ecl-field-def ((obj xml-object) name attributep)
  (declare (string name)
           (boolean attributep))
  (with-output-to-string (s)
    (format s "~4T~A ~A ~A" (as-dataset-type name) (as-ecl-field-name name) (as-ecl-xpath name attributep))
    (format s ";~%")))

;;;

(defgeneric as-ecl-record-def (obj name)
  (:documentation "Create an ECL RECORD definition from an object or array class."))

(defmethod as-ecl-record-def ((obj t) name)
  (declare (ignore obj name))
  "")

(defmethod as-ecl-record-def ((obj xml-object) name)
  (declare (string name))
  (let* ((result-str "")
         (my-str (with-output-to-string (s)
                   (register-layout-subname name)
                   (format s "~A := RECORD" (as-layout-name name))
                   ; (format s " // ~A" (max-visit-count obj))
                   (format s "~%")
                   (loop for field-name being the hash-keys of (attrs obj)
                           using (hash-value field-value)
                         do (format s "~A" (as-ecl-field-def field-value field-name t)))
                   (loop for field-name being the hash-keys of (children obj)
                           using (hash-value field-value)
                         do (let ((child-recdef (as-ecl-record-def field-value field-name)))
                              (when (string-not-equal child-recdef "")
                                (setf result-str (format nil "~A~A" result-str child-recdef)))
                              (format s "~A" (as-ecl-field-def field-value field-name nil))))
                   (format s "END;~%~%")
                   )))
    (format nil "~A~A" result-str my-str)))

;;;

(defun as-ecl-dataset-example (toplevel-obj toplevel-name toplevel-xpath)
  "Create an ECL comment containing an example DATASET() invocation."
  (declare (string toplevel-name)
           ((or string null) toplevel-xpath))
  (let* ((child-obj (first-hash-table-value (children toplevel-obj)))
         (noroot-opt (if (and (eql (type-of child-obj) 'xml-object)
                              (> (the fixnum (max-visit-count child-obj)) 1)) ", NOROOT" "")))
    (with-output-to-string (s)
      (format s "// ds := DATASET('~~data::~A', ~A, XML('~A'~A));~%~%"
              (string-downcase toplevel-name)
              (as-layout-name toplevel-name)
              (or toplevel-xpath "")
              noroot-opt))))

;;;

(defgeneric parse-attrs (obj source)
  (:documentation "Parses XML attributes and inserts base data types into OBJ."))

(defmethod parse-attrs ((obj xml-object) source)
  "Process XML tag attributes that may appear in the data."
  (flet ((handle-attrs (ns local-name qualified-name value explicitp)
           (declare (ignore ns)
                    (type (or string null) local-name qualified-name))
           (when explicitp
             (parse-simple (gethash (or local-name qualified-name) (attrs obj)) value))))
    (fxml.klacks:map-attributes #'handle-attrs source)))

(defgeneric parse-obj (obj source)
  (:documentation "Parses XML tokens into an internal object representation."))

(defmethod parse-obj ((obj t) source)
  (loop named parse
        do (let ((event (fxml.klacks:peek source)))
             (declare (type (or keyword null) event))
             (cond ((null event)
                    (return-from parse))
                   ((eql event :end-document)
                    (return-from parse))
                   ((eql event :start-element)
                    (return-from parse (parse-obj (make-instance 'xml-object) source)))
                   ((eql event :start-document)
                    (fxml.klacks:consume source))
                   ((or (eql event :dtd) (eql event :comment))
                    ;; events to ignore
                    )
                   (t
                    (error "xml2ecl: Unknown event at toplevel: (~A)" event))))))

(defmethod parse-obj ((obj xml-object) source)
  (loop named parse
        do (multiple-value-bind (event chars name) (fxml.klacks:consume source)
             (declare (type (or keyword null) event)
                      (type (or simple-string null) name chars))
             (cond ((null event)
                    (return-from parse obj))
                   ((eql event :end-document)
                    (reset-children-visits obj)
                    (return-from parse obj))
                   ((eql event :start-element)
                    (locally (declare (simple-string name))
                      (parse-complex (gethash name (children obj)) 'xml-object source)))
                   ((eql event :end-element)
                    (reset-children-visits obj)
                    (return-from parse obj))
                   ((eql event :characters)
                    (locally (declare (optimize (speed 1)))
                      (let ((text (string-trim '(#\Space #\Tab #\Newline) chars)))
                        (unless (string= text "")
                          (parse-simple (gethash *inner-text-tag* (children obj)) text)))))
                   ((or (eql event :start-document) (eql event :dtd) (eql event :comment))
                    ;; events to ignore
                    )
                   (t
                    (error "xml2ecl: Unknown event: (~A)" event))))))

;;;

(defgeneric fixup (obj)
  (:documentation "Find cases where a child dataset is referenced but the child is really
just a single attribute; those can be 'promoted' to a scalar and the child dataset removed."))

(defmethod fixup ((obj t))
  obj)

(defmethod fixup ((obj xml-object))
  (loop for name being the hash-keys of (children obj)
          using (hash-value child)
        do (if (single-inner-text-p child)
               (setf (gethash name (children obj)) (gethash *inner-text-tag*
                                                            (children (gethash name (children obj)))))
               (fixup child)))
  obj)

;;;

(defmacro with-wrapped-xml-stream ((s element-name wrapped-stream) &body body)
  "Wrap stream WRAPPED-STREAM, containing XML data, with empty tags named ELEMENT-NAME.
S should be the symbol of the stream that is created and will be referenced in the BODY."
  (let ((begin-tag-stream (gensym "begin_stream_"))
        (end-tag-stream (gensym "end_stream_"))
        (start-tag (gensym "start_tag_"))
        (end-tag (gensym "end_tag_")))
    `(let ((,start-tag (format nil "<~A>" ,element-name))
           (,end-tag (format nil "</~A>" ,element-name)))
       (flexi-streams:with-input-from-sequence (,begin-tag-stream ,start-tag :transformer #'char-code)
         (flexi-streams:with-input-from-sequence (,end-tag-stream ,end-tag :transformer #'char-code)
           (let ((,s (make-concatenated-stream ,begin-tag-stream ,wrapped-stream ,end-tag-stream)))
             ,@body))))))

(defun normalize-input-stream (xml-stream)
  "Layer a flexi-streams implementation on top of the stream and ensure that the element type
is correct.."
  (let ((local-stream (flexi-streams:make-flexi-stream xml-stream)))
    (setf (flexi-streams:flexi-stream-element-type local-stream) '(unsigned-byte 8))
    local-stream))

(defun process-stream (input obj)
  "Given a stream, wrap it in our own XML tags and then process it, stuffing the result
into OBJ."
  (let ((wrapper-tag *wrapper-xml-tag*))
    (with-wrapped-xml-stream (wrapped-stream wrapper-tag (normalize-input-stream input))
      (fxml.klacks:with-open-source (source (fxml:make-source wrapped-stream))
        (handler-bind ((fxml:well-formedness-violation #'continue))
          (setf obj (parse-obj obj source))))))
  obj)

(defmethod process-file-or-stream (input obj)
  "Convert a file to a stream for processing."
  (with-open-file (file-stream (uiop:probe-file* input)
                               :direction :input
                               :element-type '(unsigned-byte 8))
    (process-stream file-stream obj)))

(defmethod process-file-or-stream ((input stream) obj)
  (process-stream input obj))

;;;

(defmethod unwrap-parsed-object ((obj t))
  (values obj nil))

(defmethod unwrap-parsed-object ((obj xml-object))
  "After processing, we need to find the first child result object that makes sense to
be the 'root' object."
  (let ((top-obj (or (gethash *wrapper-xml-tag* (children obj)) obj))
        (top-xpath nil))
    (loop named unwrap
          while (eql (type-of top-obj) 'xml-object)
          do (multiple-value-bind (child-name child-obj) (first-hash-table-kv (children top-obj))
               (if (and (= (hash-table-count (children top-obj)) 1)
                        (eql (type-of child-obj) 'xml-object)
                        (zerop (hash-table-count (attrs top-obj))))
                   (setf top-obj child-obj
                         top-xpath (with-output-to-string (s)
                                     (when top-xpath
                                       (format s "~A/" top-xpath))
                                     (format s "~A" child-name)))
                   (return-from unwrap))))
    (values top-obj top-xpath)))

;;;; Run ---------------------------------------------------------
(defun run (args &key (string-type *ecl-string-type*))
  "Dev-level entry point."
  (let* ((argc (if (listp args) (length args) 0))
         (args (if (plusp argc) args (list *standard-input*))))
    ;; Verify that files exist
    (when (plusp argc)
      (loop for input in args
            do (unless (uiop:probe-file* input)
                 (error 'missing-file :path input))))
    (let ((*ecl-string-type* (string-upcase string-type))
          (toplevel-name (if (= argc 1)
                             (pathname-name (uiop:probe-file* (car args)))
                             (format nil "~A" (gensym "toplevel_"))))
          (result-obj nil))
      ;; Make sure the string type is recognized
      (unless (member *ecl-string-type* '("UTF8" "STRING" "VARSTRING") :test #'string-equal)
        (adopt:print-error-and-exit (format nil "Unknown string type '~A'" *ecl-string-type*)))
      ;; Parse files or standard input
      (loop for input in args
            do (let ((one-item (cond ((stringp input) (uiop:probe-file* input))
                                     ((streamp input) input)
                                     (t (error 'unknown-input :thing input)))))
                 (setf result-obj (process-file-or-stream one-item result-obj))))
      ;; Fixup XML child objects
      (setf result-obj (fixup result-obj))
      ;; Unwrap the top layer, which we manually iinserted
      (multiple-value-bind (new-result-obj top-xpath) (unwrap-parsed-object result-obj)
        ;; Emit ECL record definitions
        (setf *layout-names* nil)
        (format t "~A" (as-ecl-record-def new-result-obj toplevel-name))
        (format t "~A" (as-ecl-dataset-example new-result-obj toplevel-name top-xpath))))))

;;;; User Interface ----------------------------------------------
(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *option-help*
  (adopt:make-option 'help
                     :result-key 'help
                     :help "Display help and exit."
                     :long "help"
                     :short #\h
                     :reduce (constantly t)))

(defparameter *option-ecl-string-type*
  (adopt:make-option 'string-type
                     :result-key 'string-type
                     :parameter "STRING-TYPE"
                     :help (format nil "ECL datatype to use for XML strings; must be one of ~
                                        UTF8|STRING|VARSTRING; defaults to UTF8")
                     :long "string-type"
                     :short #\s
                     :initial-value "UTF8"
                     :reduce #'adopt:last))

(defparameter *option-group-output*
  (adopt:make-group 'output-options
                    :title "Output Options"
                    :help "These options affect how the ECL RECORD structures are created."
                    :options (list *option-ecl-string-type*)))

(defparameter *examples*
  '(("Process a single XML data file:"
     . "xml2ecl foo.xml")
    ("Process multiple specific XML data files, using STRING datatype:"
     . "xml2ecl -s STRING foo.xml bar.xml baz.xml")
    ("Process all XML files in the current directory:"
     . "xml2ecl *.xml")
    ("Process XML data coming from a file via stdin:"
     . "cat foo.xml | xml2ecl")
    ("Process a SOAP result (note the sed filter):"
     . "curl -s 'https://www.example.com/SOAP.Demo.cls' | sed -e 's/<\?.*\?>//' | xml2ecl")))

(adopt:define-string *help-text*
  "xml2ecl examines XML data and deduces the ECL RECORD definitions necessary to parse it. ~
The resulting ECL definitions are returned via standard out, suitable for piping or pasting ~
into your favorite IDE.~@
~@
XML data can be supplied as one or more files or via standard input.~@
~@
Multiple files, if provided, are parsed as if they should have the same record structure. ~
This is useful for cases where you suspect that not all XML key/value objects are fully ~
defined in one file, and other files may contain the missing data.~@
~@
ECL keywords, in general, should not be used as field names in record definitions. ~
xml2ecl will prefix those fields with 'f_' when defining those field names.  Other ~
minor changes to the field names are also made (such as converting dashes to ~
underscores).~@
~@
The last ECL record definition in the output will be the 'root' definition; it ~
is the one you should pass to the ECL DATASET() function.  If you pass exactly ~
one file to xml2ecl then that record definition will be named after the file. ~
If you pass multiple files, or stream XML data in via standard input, then the ~
layout will be named TOPLEVEL with some added items to make it unique.~@
~@
Home: https://github.com/dancamper/xml2ecl")

(defparameter *ui*
  (adopt:make-interface :name "xml2ecl"
                        :usage "[OPTIONS] [FILE...]"
                        :summary (format nil "analyze XML data and emit ECL record ~
                                              definitions that can parse that data")
                        :help *help-text*
                        :examples *examples*
                        :contents (list
                                   *option-help*
                                   *option-group-output*)))

(defun toplevel ()
  "CLI-level entry point."
  #+sbcl
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      (cond ((gethash 'help options)
             (adopt:print-help-and-exit *ui*)))
      (handler-case (run arguments :string-type (gethash 'string-type options))
        (user-error (e) (adopt:print-error-and-exit e))
        (fxml:xml-parse-error (e) (adopt:print-error-and-exit e))))))
