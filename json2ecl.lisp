(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :with-user-abort :com.inuoe.jzon) :silent t))

(defpackage :json2ecl
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :json2ecl)

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
  "The ECL data type to be used for JSON string types.  Can be overridden
with an option.")

;;;; Errors ------------------------------------------------------
(define-condition user-error (error)
  ())

(define-condition missing-file (user-error)
  ((path :initarg :path))
  (:report
   (lambda (c s)
     (format s "missing file '~A'" (slot-value c 'path)))))

;;;; Functionality -----------------------------------------------
(defclass array-item ()
  ((element-type :accessor element-type :initform nil)))

(defclass object-item ()
  ((keys :accessor keys :initform (make-hash-table :test 'equalp :size 25))))

;;;

(defun is-ecl-keyword-p (name)
  "Test if NAME (which should be a lowercase string) is an ECL keyword."
  (member name *ecl-keywords* :test 'equalp))

(defun remove-illegal-chars (name &key (replacement-char #\_) (keep-char-list '()))
  "Return a copy of NAME with characters illegal for ECL attribute names
substituted with a replacment character, then reducing runs of those
replacement characters down to a single occurrence."
  (let* ((keep-chars (reduce 'cons keep-char-list
                             :initial-value (list #\_ replacement-char)
                             :from-end t))
         (initial (substitute-if replacement-char
                                 (lambda (c) (not (or (alphanumericp c) (member c keep-chars))))
                                 (or name ""))))
    (with-output-to-string (s)
      (loop for c across initial
            with skip = nil
            do (progn
                 (unless (and (eql c replacement-char) skip)
                   (format s "~A" c))
                 (setf skip (eql c replacement-char)))))))

;;;

(defun apply-prefix (name prefix-char)
  (format nil "~A~A~A"
          prefix-char
          (if (char= (elt name 0) #\_) "" "_")
          name))

(defun legal-layout-subname (name)
  "Return a copy of NAME that can be used within a RECORD name."
  (let ((initial (string-upcase (remove-illegal-chars name))))
    (if (not (alpha-char-p (elt initial 0)))
        (apply-prefix initial "F")
        initial)))

(defun register-layout-subname (name)
  "Push layout subname NAME to a special variable list so we can track usage."
  (let ((legal-name (legal-layout-subname name)))
    (push legal-name *layout-names*)))

(defun base-type (jzon-atom)
  "Convert a JZON data type to an internal symbol to common-up some types."
  (etypecase jzon-atom
    ((eql t) 'boolean)
    ((eql nil) 'boolean)
    ((eql null) 'null-value)
    (integer (if (>= jzon-atom 0) 'pos-number 'neg-number))
    (double-float 'float)
    (string 'default-string)))

(defun common-type (new-type old-type)
  "Given two internal symbols, return an internal type that can encompass both."
  (let ((args (list new-type old-type)))
    (cond ((not old-type)
           new-type)
          ((not new-type)
           old-type)
          ((eql new-type old-type)
           new-type)
          ((member 'default-string args)
           'default-string)
          ((member 'string args)
           'string)
          ((and (member 'neg-number args)
                (member 'pos-number args))
           'neg-number)
          ((and (intersection '(neg-number pos-number) args)
                (member 'float args))
           'float)
          (t
           'string))))

(defun reduce-base-type (types)
  (reduce #'common-type types))

;;;

(defun as-layout-name (name)
  "Construct a string that is a suitable ECL RECORD attribute, based on NAME."
  (let* ((legal-name (legal-layout-subname name))
         (name-count (count-if #'(lambda (x) (equalp x legal-name)) *layout-names*))
         (interstitial (if (< name-count 2) "" (format nil "_~3,'0D" name-count))))
    (format nil "~A~A_LAYOUT" legal-name interstitial)))

(defun as-ecl-field-name (name)
  "Return a copy of NAME that is suitable to be used as an ECL attribute."
  (let* ((lowername (string-downcase name))
         (no-dashes (remove-illegal-chars lowername)))
    (if (or (not (alpha-char-p (elt no-dashes 0)))
            (is-ecl-keyword-p no-dashes))
        (apply-prefix no-dashes "f")
        no-dashes)))

(defun as-ecl-xpath (name)
  "Construct an ECL XPATH directive for NAME (typically an as-is JSON key)."
  (let ((cleaned-name (remove-illegal-chars name :replacement-char #\* :keep-char-list '(#\-))))
    (format nil "{XPATH('~A')}" cleaned-name)))

(defun as-dataset-type (name)
  "Construct an ECL DATASET datatype, given NAME."
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
                      (member (as-ecl-type value-type) '(*ecl-string-type* "STRING") :test #'string=))))
    (labels ((desc (v)
               (case v
                 (null-value "null")
                 (default-string "string")
                 (pos-number "unsigned integer")
                 (neg-number "signed integer")
                 (t (format nil "~(~A~)" v)))))
      (format nil "// ~{~A~^, ~}" (mapcar #'desc value-type)))))

;;;

(defgeneric as-ecl-field-def (value-obj name)
  (:documentation "Create an ECL field definition from an object or array class."))

(defmethod as-ecl-field-def ((value-obj t) name)
  (let ((ecl-type (as-ecl-type value-obj))
        (xpath (as-ecl-xpath name))
        (comment (as-value-comment value-obj)))
    (with-output-to-string (s)
      (format s "~4T~A ~A ~A;" ecl-type (as-ecl-field-name name) xpath)
      (when comment
        (format s " ~A" comment))
      (format s "~%"))))

(defmethod as-ecl-field-def ((obj object-item) name)
  (let ((xpath (as-ecl-xpath name)))
    (with-output-to-string (s)
      (format s "~4T~A ~A ~A" (as-dataset-type name) (as-ecl-field-name name) xpath)
      (format s ";~%"))))

(defmethod as-ecl-field-def ((obj array-item) name)
  (let ((field-name (as-ecl-field-name name))
        (xpath (as-ecl-xpath name)))
    (with-output-to-string (s)
      (if (listp (element-type obj))
          (format s "~4TSET OF ~A" (as-ecl-type (reduce-base-type (element-type obj))))
          (format s "~4T~A" (as-dataset-type name)))
      (format s " ~A ~A" field-name xpath)
      (format s ";~%"))))

;;;

(defgeneric as-ecl-record-def (obj name)
  (:documentation "Create an ECL RECORD definition from an object or array class."))

(defmethod as-ecl-record-def ((obj t) name)
  (declare (ignore obj name))
  "")

(defmethod as-ecl-record-def ((obj object-item) name)
  (let* ((result-str "")
         (my-str (with-output-to-string (s)
                   (register-layout-subname name)
                   (format s "~A := RECORD~%" (as-layout-name name))
                   (loop for field-name being the hash-keys of (keys obj)
                           using (hash-value field-value)
                         do (let ((child-recdef (as-ecl-record-def field-value field-name)))
                              (when (string/= child-recdef "")
                                (setf result-str (format nil "~A~A" result-str child-recdef)))
                              (format s "~A" (as-ecl-field-def field-value field-name))))
                   (format s "END;~%~%")
                   )))
    (format nil "~A~A" result-str my-str)))

(defmethod as-ecl-record-def ((obj array-item) name)
  (etypecase (element-type obj)
    (array-item (as-ecl-record-def (element-type obj) name))
    (object-item (as-ecl-record-def (element-type obj) name))
    (t "")))

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
            (error "json2ecl: Mismatching object types; expected ~A but found ~A"
                   (type-of ,place)
                   ,classname)))
     ,place))

(defmacro parse-simple (place value)
  "Pushes the base type of VALUE onto the sequence PLACE."
  `(unless (or (typep ,place 'array-item) (typep ,place 'object-item))
     (pushnew (base-type ,value) ,place)))

(defmacro parse-complex (place classname parser)
  "Reuse object in PLACE if possible, or create a new instance of CLASSNAME,
then kick off a new depth of parsing with the result."
  `(progn
     (reuse-object ,place ,classname)
     (parse-obj ,place ,parser nil)))

;;;

(defgeneric parse-obj (obj parser is-toplevel-p)
  (:documentation "Parses JZON-provided tokens into an internal object representation."))

(defmethod parse-obj ((obj t) parser (is-toplevel-p (eql t)))
  (loop named parse
        do (multiple-value-bind (event value) (com.inuoe.jzon:parse-next parser)
             (cond ((null event)
                    (return-from parse))
                   ((eql event :begin-array)
                    (reuse-object obj 'array-item)
                    (parse-obj obj parser nil))
                   ((eql event :begin-object)
                    (reuse-object obj 'object-item)
                    (parse-obj obj parser nil))
                   (t
                    (error "json2ecl: Unknown object at toplevel: (~A,~A)" event value)))))
  obj)

(defmethod parse-obj ((obj array-item) parser (is-toplevel-p (eql nil)))
  (loop named parse
        do (multiple-value-bind (event value) (com.inuoe.jzon:parse-next parser)
             (cond ((null event)
                    (error "json2ecl: Unexpected end of file"))
                   ((eql event :end-array)
                    (when (not (element-type obj))
                      (pushnew 'null-value (element-type obj)))
                    (return-from parse))
                   ((eql event :value)
                    (parse-simple (element-type obj) value))
                   ((eql event :begin-array)
                    (parse-complex (element-type obj) 'array-item parser))
                   ((eql event :begin-object)
                    (parse-complex (element-type obj) 'object-item parser))
                   (t
                    (error "json2ecl: Unknown object while parsing array: (~A,~A)" event value)))))
  obj)

(defmethod parse-obj ((obj object-item) parser (is-toplevel-p (eql nil)))
  (loop named parse
        do (multiple-value-bind (event value) (com.inuoe.jzon:parse-next parser)
             (cond ((null event)
                    (error "json2ecl: Unexpected end of file"))
                   ((eql event :end-object)
                    (return-from parse))
                   ((eql event :object-key)
                    (multiple-value-bind (key-event key-value) (com.inuoe.jzon:parse-next parser)
                      (case key-event
                        ((:value)
                         (parse-simple (gethash value (keys obj)) key-value))
                        ((:begin-array)
                         (parse-complex (gethash value (keys obj)) 'array-item parser))
                        ((:begin-object)
                         (parse-complex (gethash value (keys obj)) 'object-item parser))
                        (otherwise
                         (error "json2ecl: Unknown object while parsing object value: (~A,~A)"
                                key-event key-value)))))
                   (t
                    (error "json2ecl: Unknown object while parsing object: (~A,~A)" event value)))))
  obj)

;;;

(defun process-file-or-stream (input parsed-obj)
  "Entry point for parsing a single JSON data blob; INPUT can be a pathname
or a file stream; PARSED-OBJ should be a toplevel object."
  (com.inuoe.jzon:with-parser (parser input)
    (handler-case (setf parsed-obj (parse-obj parsed-obj parser t))
      (error (e) (com.inuoe.jzon::%raise 'com.inuoe.jzon:json-parse-error
                                         (slot-value parser 'com.inuoe.jzon::%pos)
                                         (format nil "~A" e)))))
  parsed-obj)

;;;; Run ---------------------------------------------------------
(defun run (args &key (string-type *ecl-string-type*))
  "Dev-level entry point."
  (let* ((argc (length args))
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
      (unless (member *ecl-string-type* '("UTF8" "STRING" "VARSTRING") :test #'string=)
        (adopt:print-error-and-exit (format nil "Unknown string type '~A'" *ecl-string-type*)))
      ;; Parse files or standard input
      (loop for input in args
            do (let ((one-item (or (uiop:probe-file* input) input)))
                 (setf result-obj (process-file-or-stream one-item result-obj))))
      ;; Emit ECL record definitions
      (setf *layout-names* nil)
      (format t "~A" (as-ecl-record-def result-obj toplevel-name)))))

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
                     :help (format nil "ECL datatype to use for JSON strings; must be one of ~
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
  '(("Process a single JSON data file:"
     . "json2ecl foo.json")
    ("Process multiple specific JSON data files, using STRING datatype:"
     . "json2ecl -s STRING foo.json bar.json baz.json")
    ("Process all JSON files in the current directory:"
     . "json2ecl *.json")
    ("Process JSON data coming from a file via stdin:"
     . "cat foo.json | json2ecl")
    ("Process a REST result:"
     . "curl -s 'https://jsonplaceholder.typicode.com/todos' | json2ecl")))

(adopt:define-string *help-text*
  "json2ecl examines JSON data and deduces the ECL RECORD definitions necessary to parse it. ~
The resulting ECL definitions are returned via standard out, suitable for piping or pasting ~
into your favorite IDE.~@
~@
JSON data can be supplied as one or more files or via standard input.~@
~@
Multiple files, if provided, are parsed as if they should have the same record structure. ~
This is useful for cases where you suspect that not all JSON key/value objects are fully ~
defined in one file, and other files may contain the missing data.~@
~@
ECL keywords, in general, should not be used as field names in record definitions. ~
json2ecl will prefix those fields with 'f_' when defining those field names.  Other ~
minor changes to the field names are also made (such as converting dashes to ~
underscores).~@
~@
The last ECL record definition in the output will be the 'root' definition; it ~
is the one you should pass to the ECL DATASET() function.  If you pass exactly ~
one file to json2ecl then that record definition will be named after the file. ~
If you pass multiple files, or stream JSON data in via standard input, then the ~
layout will be named TOPLEVEL with some added items to make it unique.")

(defparameter *ui*
  (adopt:make-interface :name "json2ecl"
                        :usage "[OPTIONS] [FILE...]"
                        :summary (format nil "analyze JSON data and emit ECL record ~
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
        (com.inuoe.jzon:json-error (e) (adopt:print-error-and-exit e))))))
        
