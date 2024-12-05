(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :alexandria :asdf :uiop :with-user-abort) :silent t))

(defpackage :ecl-extract-functions
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :ecl-extract-functions)

; (declaim (optimize (speed 0) (space 0) (debug 3)))

;;;; Configuration -----------------------------------------------

(defvar *output-prefix* "")
(defvar *searcher* nil)
(defparameter *case-insensitive-p* nil)
(defparameter *string-compare-fn* #'string=)
(defparameter *char-compare-fn* #'char=)
(defparameter *recursive-search-p* nil)
(defparameter *tab-delimited-output* nil)

;;;; Errors ------------------------------------------------------

(define-condition user-error (error) ())

(define-condition missing-function-name (user-error) ()
  (:report "At least one function name is required, but none was supplied."))

;;;; Functionality -----------------------------------------------

(defclass tracked-stream ()
  ((str :initarg :stream :reader str)
   (line-num :initform 1 :reader line-num :type fixnum)))

(defmethod read-character ((s tracked-stream))
  (with-slots (str line-num) s
    (let ((c (read-char str nil)))
      (case c
        ((#\Newline #\Linefeed)
         (incf line-num)))
      c)))

(defmethod unread-character ((c character) (s tracked-stream))
  (with-slots (str line-num) s
    (case c
        ((#\Newline #\Linefeed)
         (decf line-num)))
    (unread-char c str)))

(defmethod peek-character ((s tracked-stream))
  (with-slots (str line-num) s
    (peek-char nil str nil)))

;;; -------------------------------

(declaim (inline whitespace-p))
(defun whitespace-p (c)
  (or (char= #\Space c)
      (not (graphic-char-p c))))

(declaim (inline skip-whitespace-and-char))
(defun skip-whitespace-and-char (s skip-char)
  (declare (tracked-stream s)
           (character skip-char))
  (loop :for c = (read-character s)
        :while c
        :do (cond ((char= c skip-char)
                   (return c))
                  ((not (whitespace-p c))
                   (unread-character c s)
                   (return nil)))))

(declaim (inline string-starts-with-p))
(defun string-starts-with-p (target prefix &key (test *string-compare-fn*))
  "Does target begin with prefix?"
  (declare (string target prefix))
  (let ((pos (search prefix target :test test)))
    (and pos (zerop pos))))

(defun make-tab-delimited-string (alist)
  (let ((s ""))
    (loop :for e :in alist
          :do (progn
                (when (plusp (length s))
                  (setf s (concatenate 'string s (string #\Tab))))
                (setf s (concatenate 'string s (format nil "~A" e)))))
    s))

;;; -------------------------------

(defstruct trie-node
  (next (make-hash-table :test #'eql))
  (wordp nil))

;;; -------------------------------

(defclass searcher ()
  ((names :initarg :names :accessor names)
   (min-length :initform 0 :accessor min-length :type fixnum)
   (max-length :initform 0 :accessor max-length :type fixnum)
   (scan-buffer :accessor scan-buffer :type basic-string)
   (arg-buffer :accessor arg-buffer :type (simple-array character (*)))
   (skip-map :accessor skip-map)
   (trie :accessor trie)))

(defun make-searcher (function-name-list)
  (let ((obj (make-instance 'searcher)))
    (with-slots (names min-length max-length scan-buffer arg-buffer skip-map trie) obj
      (setf names (mapcar #'reverse function-name-list)
            min-length (apply #'min (mapcar #'length function-name-list))
            max-length (apply #'max (mapcar #'length function-name-list))
            scan-buffer (make-string max-length :element-type 'character :initial-element #\Space)
            arg-buffer (make-array 1024 :element-type 'character :fill-pointer 0 :adjustable t)
            skip-map (make-hash-table :test #'eql :size (* 2 max-length (length names)))
            trie (make-trie-node))
      (let ((child-node trie))
        (labels ((process-char (c i)
                   (let ((skip-value (min i (gethash c skip-map min-length)))
                         (next-child (gethash c (trie-node-next child-node))))
                     (setf (gethash c skip-map) skip-value)
                     (unless next-child
                       (setf next-child (make-trie-node)
                             (gethash c (trie-node-next child-node)) next-child))
                     (setf child-node next-child)))
                 (process-name (name)
                   (setf child-node trie)
                   (loop :for i :from 0 :upto (1- (length name))
                         :do (process-char (char name i) i))
                   (setf (trie-node-wordp child-node) t)))
          (dolist (name names)
            (if *case-insensitive-p*
                (process-name (string-upcase name))
                (process-name name))))))
    obj))

(defmethod reset ((obj searcher))
  (with-slots (scan-buffer max-length) obj
   (setf scan-buffer (make-string max-length :element-type 'character :initial-element #\Space)) ))

(defmethod skip-length ((obj searcher) (c character))
  (with-slots (min-length skip-map) obj
    (gethash c skip-map min-length)))

;;; Assumes stream has just read the open parenthesis for the arguments
(defmethod extract-arguments ((obj searcher) (s tracked-stream))
  (let* ((arg-list '())
         (bracket-count 0)
         (paren-count 1)
         (within-quotes nil)
         (within-line-comment nil)
         (within-block-comment nil))
    (declare (fixnum bracket-count paren-count))
    (with-slots (arg-buffer) obj
      (setf (fill-pointer arg-buffer) 0)
      (loop :named arg-scan-loop
            :for c = (read-character s)
            :while c
            :do (let ((add-char t))
                  (cond ((char= c #\\)  ; begin backslash
                         (vector-push-extend c arg-buffer)
                         (setf c (read-character s))
                         (unless c
                           (return-from arg-scan-loop)))
                        (within-quotes
                         (cond ((char= c #\')
                                (setf within-quotes nil))
                               ((whitespace-p c)
                                (setf c #\Space))))
                        (within-line-comment
                         (when (or (char= c #\Linefeed)
                                   (char= c #\Newline))
                           (setf within-line-comment nil))
                         (setf add-char nil))
                        (within-block-comment
                         (let ((next-char (peek-character s)))
                           (when (and next-char
                                      (char= c #\*)
                                      (char= next-char #\/))
                             (setf within-block-comment nil
                                   c (read-character s))
                             (unless c
                               (return-from arg-scan-loop))))
                         (setf add-char nil))
                        ((char= c #\/)
                         (let ((next-char (peek-character s)))
                           (cond ((and next-char
                                       (char= next-char #\/))
                                  (setf within-line-comment t
                                        add-char nil
                                        c (read-character s))
                                  (unless c
                                    (return-from arg-scan-loop)))
                                 ((and next-char
                                       (char= next-char #\*))
                                  (setf within-block-comment t
                                        add-char nil
                                        c (read-character s))
                                  (unless c
                                    (return-from arg-scan-loop))))))
                        ((char= c #\')
                         (setf within-quotes t))
                        ((char= c #\[)
                         (incf bracket-count))
                        ((char= c #\])
                         (decf bracket-count))
                        ((and (char= c #\()
                              (zerop bracket-count))
                         (incf paren-count))
                        ((and (char= c #\))
                              (zerop bracket-count))
                         (decf paren-count)
                         (when (zerop paren-count)
                           (unless (zerop (length arg-buffer))
                             (push (subseq arg-buffer 0 (length arg-buffer)) arg-list))
                           (return-from arg-scan-loop)))
                        ((whitespace-p c)
                         (setf add-char nil))
                        ((and (char= c #\,)
                              (= paren-count 1)
                              (zerop bracket-count))
                         (push (subseq arg-buffer 0 (length arg-buffer)) arg-list)
                         (setf (fill-pointer arg-buffer) 0
                               add-char nil)))
                  (when add-char
                    (vector-push-extend c arg-buffer)))))
    (nreverse arg-list)))

(declaim (inline push-char-onto-fixed-string))
(defun push-char-onto-fixed-string (c str)
  "Treat the string as a queue of characters; push the new character onto
the head of the string and push the other characters down the queue, removing
the last character; str is destructively modified."
  (declare (character c)
           (simple-string str)
           (optimize (safety 0) (speed 3)))
  (when (> (length str) 1)
    (loop :for i :from (- (length str) 2) :downto 0
          :do (setf (char str (1+ i)) (char str i))))
  (setf (char str 0) c))

(defmethod extract-function-and-arguments ((obj searcher) (s stream))
  (let ((str (make-instance 'tracked-stream :stream s))
        (within-line-comment nil)
        (within-block-comment nil)
        (skip-chars 0))
    (declare (fixnum skip-chars)
             (inline push-char-onto-fixed-string
                     string-starts-with-p))
    (loop :named function-scan-loop
          :for c = (read-character str)
          :while c
          :do (with-slots (names min-length max-length scan-buffer trie) obj
                (push-char-onto-fixed-string c scan-buffer)
                (let ((ch (if *case-insensitive-p* (char-upcase c) c)))
                  (cond (within-line-comment
                         (when (or (char= c #\Linefeed)
                                   (char= c #\Newline))
                           (setf within-line-comment nil)))
                        (within-block-comment
                         (let ((next-char (peek-character str)))
                           (when (and next-char
                                      (char= c #\*)
                                      (char= next-char #\/))
                             (setf within-block-comment nil)
                             (setf c (read-character str))
                             (unless c
                               (return-from function-scan-loop)))))
                        ((char= c #\/)
                         (let ((next-char (peek-character str)))
                           (cond ((and next-char
                                       (char= next-char #\/))
                                  (setf within-line-comment t
                                        skip-chars 0)
                                  (setf c (read-character str))
                                  (unless c
                                    (return-from function-scan-loop)))
                                 ((and next-char
                                       (char= next-char #\*))
                                  (setf within-block-comment t
                                        skip-chars 0)
                                  (setf c (read-character str))
                                  (unless c
                                    (return-from function-scan-loop))))))
                        ((plusp skip-chars)
                         (decf skip-chars))
                        ((plusp (skip-length obj ch))
                         (setf skip-chars (1- (skip-length obj ch))))
                        (t
                         (let ((name-node trie)
                               (name-length 0))
                           (loop :named name-scan
                                 :for i :from 0 :upto (1- max-length)
                                 :do (let ((ch (if *case-insensitive-p* (char-upcase (char scan-buffer i)) (char scan-buffer i))))
                                       (setf name-node (gethash ch (trie-node-next name-node)))
                                       (unless name-node
                                         (return-from name-scan))
                                       (when (trie-node-wordp name-node)
                                         (setf name-length (1+ i)))))
                           (when (and (plusp name-length)
                                      (skip-whitespace-and-char str #\())
                             (let* ((line (line-num str))
                                    (args (extract-arguments obj str)))
                               (if *tab-delimited-output*
                                   (format *standard-output* "~A~C~D~C~A~C~A~%"
                                           *output-prefix*
                                           #\Tab
                                           line
                                           #\Tab
                                           (string-upcase (reverse (subseq scan-buffer 0 name-length)))
                                           #\Tab
                                           (make-tab-delimited-string args))
                                   (format *standard-output* "~A:~D:~A(~{~A~^, ~})~%"
                                           *output-prefix*
                                           line
                                           (reverse (subseq scan-buffer 0 name-length))
                                           args))
                               (setf skip-chars min-length)))))))))))

;;;; Run ---------------------------------------------------------

(defun relative-path (path cwd)
  (if (string-starts-with-p path cwd)
      (subseq path (length cwd))
      path))

(defun run% (input)
  (reset *searcher*)
  (handler-case
      (extract-function-and-arguments *searcher* input)
    (error (e)
      (format *error-output* "Error: ~A~%" e))))

(defmethod process-paths ((paths (eql nil)))
  (run% *standard-input*))

(defmethod process-paths ((paths list))
  (let ((cwd (namestring (uiop:getcwd))))
    (dolist (path (mapcar #'uiop:probe-file* paths))
      (when path
        (if (uiop:directory-exists-p (namestring path))
            (when *recursive-search-p*
              (let ((sub-paths (append (uiop:directory-files path)
                                       (uiop:subdirectories path))))
                (when sub-paths
                  (process-paths (mapcar #'namestring sub-paths)))))
            (when (string= "ecl" (pathname-type path))
              (with-open-file (s path :direction :input :external-format :utf-8)
                (let ((*output-prefix* (format nil "~A" (relative-path (namestring path) cwd))))
                  (declare (special *output-prefix*))
                  (run% s)))))))))

(defmethod run ((function-name-or-list list) (paths (eql nil)))
  (setf *searcher* (make-searcher function-name-or-list))
  (run% *standard-input*))

(defmethod run ((function-name-or-list list) (paths list))
  (setf *searcher* (make-searcher function-name-or-list))
  (process-paths paths))

;;;; User Interface ----------------------------------------------

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *option-help*
  (adopt:make-option 'help
                     :result-key 'help
                     :help "Display help and exit"
                     :long "help"
                     :short #\h
                     :reduce (constantly t)))

(defparameter *option-case-insensitive*
  (adopt:make-option 'case-insensitive
                     :help "Search case-insensitive"
                     :short #\i
                     :initial-value nil
                     :reduce (constantly t)))

(defparameter *option-recursive-file-search*
  (adopt:make-option 'recursive-file-search
                     :help "Recursively process files within subdirectories"
                     :short #\r
                     :initial-value nil
                     :reduce (constantly t)))

(defparameter *option-tab-delimited-output*
  (adopt:make-option 'tab-delimited-output
                     :help "Output results in tab-delimited fields"
                     :short #\t
                     :initial-value nil
                     :reduce (constantly t)))

(defparameter *function-name-list*
  (adopt:make-option 'function-name-list
                     :help "ECL function name to search for; this option may be repeated to search for multiple functions"
                     :short #\p
                     :parameter "FUNCTION"
                     :initial-value nil
                     :reduce #'adopt:collect))

(defparameter *ui*
  (adopt:make-interface
   :name "ecl-extract-functions"
   :summary "Extract ECL functions and their arguments from ECL code."
   :usage "[OPTIONS] -p FUNCTION [-p FUNCTION...] [FILE...]"
   :help "Search for ECL function invocations in source code and extract their arguments"
   :contents (list *option-help*
                   *option-case-insensitive*
                   *option-recursive-file-search*
                   *option-tab-delimited-output*
                   *function-name-list*)))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      (handler-case
          (cond ((gethash 'help options)
                 (adopt:print-help-and-exit *ui*))
                ((null (gethash 'function-name-list options))
                 (error 'missing-function-name)))
        (user-error (e) (adopt:print-error-and-exit e)))
      (when (gethash 'case-insensitive options)
        (setf *string-compare-fn* #'string-equal
              *char-compare-fn* #'char-equal
              *case-insensitive-p* t))
      (when (gethash 'recursive-file-search options)
        (setf *recursive-search-p* t))
      (when (gethash 'tab-delimited-output options)
        (setf *tab-delimited-output* t))
      (run (gethash 'function-name-list options) arguments))))

