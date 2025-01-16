(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :cl-ppcre :flexi-streams :with-user-abort) :silent t))

(defpackage :tailf
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :tailf)

;;;; Configuration -----------------------------------------------

(defun rgb-code (r g b)
  (+ (* r 36) (* g 6) (* b 1) 16))

(defun make-colors (excludep)
  (let ((result (make-array 256 :fill-pointer 0)))
    (dotimes (r 6)
      (dotimes (g 6)
        (dotimes (b 6)
          (unless (funcall excludep (+ r g b))
            (vector-push-extend (rgb-code r g b) result)))))
    result))

(defparameter *colors-for-dark-terminal*  (make-colors (lambda (v) (< v 3))))
(defparameter *colors-light-terminal* (make-colors (lambda (v) (> v 11))))
(defvar *colors* *colors-for-dark-terminal*) ; default to dark

;;;; Errors ------------------------------------------------------

(define-condition user-error (error) ())

;;;; Functionality -----------------------------------------------

(defun djb2 (string)
  ;; http://www.cse.yorku.ca/~oz/hash.html
  (reduce (lambda (hash c) (mod (+ (* 33 hash) c) (expt 2 64)))
          string
          :initial-value 5381
          :key #'char-code))

;;; --------------------------

(defun find-color (string)
  (aref *colors* (mod (djb2 string) (length *colors*))))

(defun ansi-color-start (color)
  (format nil "~C[38;5;~Dm" #\Escape color))

(defun ansi-color-end ()
  (format nil "~C[0m" #\Escape))

;;; --------------------------

(defun start-colorizing (string)
  (format *standard-output* "~A" (ansi-color-start (find-color string))))

(defun stop-colorizing ()
  (format *standard-output* "~A" (ansi-color-end)))

;;;; Run ---------------------------------------------------------

(defun run (paths)
  (when paths
    (let ((scanner (ppcre:create-scanner "==> (.+?) <=="))
          (launch-args (append (list "tail" "-F") (if (listp paths) paths (list paths)))))
      (let* ((launch-info (uiop:launch-program launch-args :output :stream))
             (raw-input-stream (uiop:process-info-output launch-info))
             (input-stream (flexi-streams:make-flexi-stream raw-input-stream)))
        (setf (flexi-streams:flexi-stream-element-type input-stream) '(unsigned-byte 8))
        (loop :for line = (read-line input-stream nil nil)
              :while line
              :do (multiple-value-bind (ms me rs re) (ppcre:scan scanner line)
                    (declare (ignore ms me))
                    (when (plusp (length rs))
                      (start-colorizing (subseq line (aref rs 0) (aref re 0))))
                    (write-line line *standard-output*)))))))

;;;; User Interface ----------------------------------------------

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort ()
       (progn
         (stop-colorizing)
         (sb-ext:exit :code 130)))))

(defparameter *option-help*
  (adopt:make-option 'help
                     :result-key 'help
                     :help "Display help and exit"
                     :long "help"
                     :short #\h
                     :reduce (constantly t)))

(defparameter *option-dark-terminal*
  (adopt:make-option 'dark-terminal
                     :help "Use font colors suitable for a dark terminal background"
                     :long "dark"
                     :short #\D
                     :reduce (constantly t)))

(defparameter *option-light-terminal*
  (adopt:make-option 'light-terminal
                     :help "Use font colors suitable for a light terminal background"
                     :long "light"
                     :short #\L
                     :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
   :name "tailf"
   :summary "Wraps the tail command line utility, specifically when tailing multiple files. Colorize the output from each file differently."
   :usage "[OPTIONS] FILE [FILE]+"
   :help ""
   :contents (list *option-help*
                   *option-dark-terminal*
                   *option-light-terminal*)))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      (handler-case
          (cond ((gethash 'help options)
                 (adopt:print-help-and-exit *ui*))
                (t
                 (cond ((gethash 'dark-terminal options)
                        (setf *colors* *colors-for-dark-terminal*))
                       ((gethash 'light-terminal options)
                        (setf *colors* *colors-light-terminal*)))
                 (run arguments)))
        (user-error (e) (adopt:print-error-and-exit e))))))
