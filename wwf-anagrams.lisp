(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :with-user-abort :cl-ppcre) :silent t))

(defpackage :wwf-anagrams
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :wwf-anagrams)

;;;; Configuration -----------------------------------------------
(defvar *word-hash-map* nil)
(defvar *max-word-length* 0)
(defparameter +min-word-length+ 2)
(defparameter +word-path+ "~/Documents/wwf_scrabble.txt")

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

;;;; Functionality -----------------------------------------------


;;;; Run ---------------------------------------------------------
(defun prime-factor-from-char (ch)
  (case ch
    (#\E 2)
    (#\A 3)
    (#\R 5)
    (#\I 7)
    (#\O 11)
    (#\T 13)
    (#\N 17)
    (#\S 19)
    (#\L 23)
    (#\C 29)
    (#\U 31)
    (#\D 37)
    (#\P 41)
    (#\M 43)
    (#\H 47)
    (#\G 53)
    (#\B 59)
    (#\F 61)
    (#\Y 67)
    (#\W 71)
    (#\K 73)
    (#\V 79)
    (#\X 83)
    (#\Z 89)
    (#\J 97)
    (#\Q 101)
    (t 1)))

(defun prime-factor-product-from-word (word &optional (seed 1))
  (let ((pfp seed))
    (loop :for c :across word
          :do (setf pfp (* pfp (prime-factor-from-char c))))
    pfp))

(defun scrabble-score-from-char (ch)
  (case ch
    (#\A 1)
    (#\B 4)
    (#\C 4)
    (#\D 2)
    (#\E 1)
    (#\F 4)
    (#\G 3)
    (#\H 3)
    (#\I 1)
    (#\J 10)
    (#\K 5)
    (#\L 2)
    (#\M 4)
    (#\N 2)
    (#\O 1)
    (#\P 4)
    (#\Q 10)
    (#\R 1)
    (#\S 1)
    (#\T 1)
    (#\U 2)
    (#\V 5)
    (#\W 4)
    (#\X 8)
    (#\Y 3)
    (#\Z 10)
    (t 0)))

(defun scrabble-score-from-word (word)
  (loop :for c across word
        :sum (scrabble-score-from-char c) into score
        :finally (return score)))

(defun create-dictionary-map ()
  (setf *word-hash-map* (make-hash-table :size 200000 :test #'eql)
        *max-word-length* 0)
  (with-open-file (word-file +word-path+)
    (loop :for line = (read-line word-file nil)
          :while line
          :if (>= (length line) +min-word-length+)
            do (let* ((one-word (string-upcase line))
                      (pfp (prime-factor-product-from-word one-word)))
                 (when (plusp pfp)
                   (setf *max-word-length* (max *max-word-length* (length one-word)))
                   (pushnew one-word
                            (gethash pfp *word-hash-map*)
                            :test #'string=)))))
  *word-hash-map*)

(defun ensure-dictionary-map ()
  (or *word-hash-map* (create-dictionary-map)))

(defun pfp-list-from-subseq (word len &optional (prefix ""))
  (declare (fixnum len))
  (let ((result ()))
    (if (= len 1)
        (let ((prefix-pfp (prime-factor-product-from-word prefix)))
          (loop :for c :across word
                :do (let ((pfp (* prefix-pfp (prime-factor-from-char c))))
                      (when (plusp pfp)
                        (pushnew pfp result :test #'eql)))))
        (dotimes (i (1- (length word)))
          (let ((new-prefix (format nil "~A~A" prefix (char word i)))
                (new-word  (subseq word (1+ i))))
            (setf result (union result
                                (pfp-list-from-subseq new-word (1- len) new-prefix)
                                :test #'equalp)))))
    result))

(defun all-inner (letter-list min-length max-length)
  (declare (fixnum min-length max-length))
  (let ((result-hash (make-hash-table :size 1000 :test #'eql))
        (word-min (max +min-word-length+ min-length))
        (word-max (min (apply #'max (mapcar #'length letter-list)) max-length)))
    (loop :for l :in letter-list
          :do (dotimes (x (1+ (- word-max word-min)))
                (dolist (pfp (pfp-list-from-subseq l (+ x word-min)))
                  (when (plusp pfp)
                    (let ((dictionary-value (gethash pfp (ensure-dictionary-map))))
                      (when dictionary-value
                        (setf (gethash pfp result-hash) (gethash pfp (ensure-dictionary-map)))))))))
    (loop :for v :being :the :hash-values :in result-hash
          :append v)))

(defun clean-letters (letters)
  (let ((cleaned-letters (string-upcase letters)))
    (values (string (remove-if #'(lambda (c) (char-equal #\. c)) cleaned-letters))
            (string (remove-if-not #'(lambda (c) (char-equal #\. c)) cleaned-letters)))))

(defun create-letter-list (base-letters blank-count)
  (let ((letter-list '()))
    (if (zerop blank-count)
        (push base-letters letter-list)
        (progn
          (loop :for c :across "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                :do (setf letter-list (append (create-letter-list (concatenate 'string base-letters (string c)) (1- blank-count))
                                              letter-list)))))
    letter-list))

(defun filter-on-regex (word-list regex)
  (if regex
      (remove-if-not #'(lambda (x) (cl-ppcre:scan regex x)) word-list)
      word-list))

(defun run (letters regex-pattern)
  (let ((min-length +min-word-length+)
        (max-length (if (plusp *max-word-length*)
                        (min *max-word-length* (length letters))
                        (length letters)))
        (regex (and regex-pattern (cl-ppcre:create-scanner regex-pattern :case-insensitive-mode t))))
    (declare (fixnum min-length max-length))
    (flet ((my-lessp (w1 w2)
             (or (< (length w1) (length w2))
                 (and (= (length w1) (length w2))
                      (string-lessp w1 w2)))))
      (multiple-value-bind (cleaned-word blanks) (clean-letters letters)
        (let ((letter-list (create-letter-list cleaned-word (length blanks))))
          (let* ((all-anagrams (all-inner letter-list min-length max-length))
                 (filtered-anagrams (filter-on-regex all-anagrams regex))
                 (word-list (sort filtered-anagrams #'my-lessp))
                 (prev-len 0))
            (dolist (w word-list)
              (when (/= (length w) prev-len)
                (format t "~%")
                (setf prev-len (length w)))
              (format t "~A ~A~%" w (scrabble-score-from-word w)))
            (format t "~%~:D words found" (length word-list))))))))

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

(defparameter *ui*
  (adopt:make-interface
   :name "wwf-anagrams"
   :summary "Create anagrams from letters, using the dictionary from Words With Friends."
   :usage "LETTERS [REGEX]"
   :help "LETTERS should be a string comprised of ASCII letters. A period can be used to represent 'any letter' (a blank, in Scrabble parlance). REGEX is optional and should be a case-insensitive regex pattern that can be used to filter the results."
   :contents (list *option-help*)))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      (when (gethash 'help options nil)
        (adopt:print-help-and-exit *ui*))
      (handler-case (run (first arguments) (second arguments))
        (user-error (e) (adopt:print-error-and-exit e))))))
        
