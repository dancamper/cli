(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :alexandria :flexi-streams :random-state :with-user-abort) :silent t))

(defpackage :tailf
  (:use :cl)
  (:local-nicknames (#:a #:alexandria))
  (:export :toplevel *ui*))

(in-package :tailf)

;;;; Configuration -----------------------------------------------

(defparameter +color-count+ 200)

(defvar *terminal-color-opt* :dark)
(defvar *colors* nil)
(defvar *color-map* (make-hash-table :test #'equalp))
(defvar *my-random-state* nil)

;;;; Errors ------------------------------------------------------

(define-condition user-error (error) ())

;;;; Functionality -----------------------------------------------

(defun raw-colors ()
  (let ((colors nil))
    (loop :for c :from 0 :to (expt 255 3) :by (/ (expt 255 3) +color-count+)
          :do (push (format nil "#~6,'0X" (round c)) colors))
    colors))

(defun parse-hex-color (hex-color)
  ;; assumes that hex-color begins with #\#
  (values (parse-integer (subseq hex-color 1 3) :radix 16)
          (parse-integer (subseq hex-color 3 5) :radix 16)
          (parse-integer (subseq hex-color 5 7) :radix 16)))

(defun rgb-luminosity (r g b)
  (flet ((gamma-color (c)
           (let ((c1 (/ c 255)))
             (if (<= c1 0.03928)
                 (/ c1 12.92)
                 (expt (/ (+ c1 0.055) 1.055) 2.4)))))
    (+ (* (gamma-color r) 0.2126)
       (* (gamma-color g) 0.7152)
       (* (gamma-color b) 0.0722))))

(defun good-contrast-p (light-luminosity dark-luminosity)
  (let ((contrast (/ (+ light-luminosity 0.05)
                     (+ dark-luminosity 0.05))))
    (>= contrast 3.0)))

(defparameter +dark-luminosity+ (rgb-luminosity 21 25 30)) ; not quite black
(defparameter +light-luminosity+ (rgb-luminosity 219 219 219)) ; not quite white

(defun include-color-p (r g b)
  (let ((luminosity (rgb-luminosity r g b)))
    (case *terminal-color-opt*
      (:dark (good-contrast-p luminosity +dark-luminosity+))
      (:light (good-contrast-p +light-luminosity+ luminosity)))))

(defun assign-colors ()
  (setf *colors* nil)
  (loop :for hex-color :in (raw-colors)
        :do (multiple-value-bind (r g b) (parse-hex-color hex-color)
              (when (include-color-p r g b)
                (push (list r g b) *colors*))))
  *colors*)

(defun shuffle-colors ()
  (loop :for x :to (length *colors*)
        :do (let ((i (random-state:random-int *my-random-state* 0 (1- (length *colors*))))
                  (j (random-state:random-int *my-random-state* 0 (1- (length *colors*)))))
              (unless (= i j)
                (rotatef (nth i *colors*) (nth j *colors*))))))

(defun find-color (string)
  (or (gethash string *color-map*)
      (setf (gethash string *color-map*) (pop *colors*))))

(defun ansi-color-end ()
  (format nil "~C[0m" #\Escape))

(defun ansi-color-start (color)
  (if color
      (destructuring-bind (r g b) color
        (format nil "~C[38;2;~D;~D;~Dm" #\Escape r g b))
      (ansi-color-end)))

(defun start-colorizing (string)
  (when *colors*
    (format *standard-output* "~A" (ansi-color-start (find-color string)))))

(defun stop-colorizing ()
  (format *standard-output* "~A" (ansi-color-end)))

;;;; Run ---------------------------------------------------------

(defun show-colors ()
  (let ((sorted-list (sort (copy-list (raw-colors)) #'< :key (lambda (c) (parse-integer (subseq c 1 7) :radix 16))))
        (omitted-count 0))
    (loop :for hex-color :in sorted-list
          :do (multiple-value-bind (r g b) (parse-hex-color hex-color)
                (unless (include-color-p r g b)
                  (format *standard-output* "; ")
                  (incf omitted-count))
                (format *standard-output* "~A" (ansi-color-start (list r g b)))
                (format  *standard-output* "~A~%" hex-color)))
    (format *standard-output* "~A" (ansi-color-end))
    (format *standard-output* "~D usable (~D omitted)~%" (- (length sorted-list) omitted-count) omitted-count)))

(defun run (paths)
  (when paths
    (let ((launch-args (append (list "tail" "-F") (if (listp paths) paths (list paths)))))
      (let* ((launch-info (uiop:launch-program launch-args :output :stream))
             (raw-input-stream (uiop:process-info-output launch-info))
             (input-stream (flexi-streams:make-flexi-stream raw-input-stream)))
        (setf (flexi-streams:flexi-stream-element-type input-stream) '(unsigned-byte 8)
              *my-random-state* (random-state:make-generator :mersenne-twister-32 (get-universal-time)))
        (assign-colors)
        (shuffle-colors)
        (loop :for line = (read-line input-stream nil nil)
              :while line
              :do (progn
                    (when (and (> (length line) 8)
                               (string= line "==> " :end1 4)
                               (string= line " <==" :start1 (- (length line) 4) :end1 (length line)))
                      (start-colorizing line))
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
                     :result-key 'color
                     :help "Use font colors suitable for a dark terminal background (the default)"
                     :long "dark"
                     :short #\D
                     :reduce (constantly 'dark)))

(defparameter *option-light-terminal*
  (adopt:make-option 'light-terminal
                     :result-key 'color
                     :help "Use font colors suitable for a light terminal background"
                     :long "light"
                     :short #\L
                     :reduce (constantly 'light)))

(defparameter *option-test-colors*
  (adopt:make-option 'test-colors
                     :result-key 'test-colors
                     :help "Debugging: Display the color values as colored text; use -L and -D to display colors for Light and Dark terminals"
                     :long "test"
                     :short #\t
                     :reduce (constantly 'test-colors)))

(defparameter *ui*
  (adopt:make-interface
   :name "tailf"
   :summary "Wraps the tail command line utility, specifically when tailing multiple files. Colorize the output from each file differently."
   :usage "[OPTIONS] FILE [FILE]+"
   :help "FILE can be any file glob acceptable to the tail command line utility."
   :contents (list *option-help*
                   *option-dark-terminal*
                   *option-light-terminal*
                   *option-test-colors*)))

(defun process-color-options (options)
  (cond ((eql (gethash 'color options) 'dark)
         (setf *terminal-color-opt* :dark))
        ((eql (gethash 'color options) 'light)
         (setf *terminal-color-opt* :light))
        (t
         (setf *terminal-color-opt* :dark))))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      (handler-case
          (cond ((gethash 'help options)
                 (adopt:print-help-and-exit *ui*))
                ((gethash 'test-colors options)
                 (process-color-options options)
                 (show-colors))
                (t
                 (process-color-options options)
                 (run arguments)))
        (user-error (e) (adopt:print-error-and-exit e))))))
