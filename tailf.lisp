(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :alexandria :flexi-streams :with-user-abort) :silent t))

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

;;;; Errors ------------------------------------------------------

(define-condition user-error (error) ())

;;;; Functionality -----------------------------------------------

(defun rgb-to-lab (r g b)
  (labels ((srgb-to-linear (c)
             (if (<= c 0.04045d0)
                 (/ c 12.92d0)
                 (expt (/ (+ c 0.055d0) 1.055d0) 2.4d0)))
           (f (v)
             (let* ((eps (expt (/ 6.0d0 29.0d0) 3)) ; ≈ 0.008856
                    (k (/ 1.0d0 3.0d0))
                    (a (expt (/ 29.0d0 6.0d0) 2)) ; ≈ 7.787^2 but exact form
                    (b (/ 4.0d0 29.0d0)))
               (if (> v eps)
                   (expt v k)
                   (+ (* a v) b)))))
    (let* ((r-n (/ r 255.0d0))
           (g-n (/ g 255.0d0))
           (b-n (/ b 255.0d0))
           ;; 1) gamma-expand to linear light
           (rl (srgb-to-linear r-n))
           (gl (srgb-to-linear g-n))
           (bl (srgb-to-linear b-n))
           ;; 2) linear RGB -> XYZ (D65, 2°) in 0..1
           (x (+ (* 0.4124564d0 rl) (* 0.3575761d0 gl) (* 0.1804375d0 bl)))
           (y (+ (* 0.2126729d0 rl) (* 0.7151522d0 gl) (* 0.0721750d0 bl)))
           (z (+ (* 0.0193339d0 rl) (* 0.1191920d0 gl) (* 0.9503041d0 bl)))
           ;; 3) scale to 0..100 to match reference white constants
           (X (* 100.0d0 x))
           (Y (* 100.0d0 y))
           (Z (* 100.0d0 z))
           ;; D65 reference white (2°)
           (Xn 95.047d0) (Yn 100.000d0) (Zn 108.883d0)
           (fx (f (/ X Xn)))
           (fy (f (/ Y Yn)))
           (fz (f (/ Z Zn)))
           (L (- (* 116.0d0 fy) 16.0d0))
           (a (* 500.0d0 (- fx fy)))
           (b (* 200.0d0 (- fy fz))))
      (values L a b))))

(defun delta-e-76 (L1 a1 b1 L2 a2 b2)
  (let ((dL (- L1 L2))
        (da (- a1 a2))
        (db (- b1 b2)))
    (sqrt (+ (* dL dL) (* da da) (* db db)))))

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
    ;; Contrast >= 4.5 is best for accessibility, but we can
    ;; use something lower for readability
    (>= contrast 3.0)))

(defparameter +dark-luminosity+ (rgb-luminosity 0 0 0)) ; black
(defparameter +light-luminosity+ (rgb-luminosity 255 255 255)) ; white

(defun allowed-contrast-p (r g b)
  (let ((luminosity (rgb-luminosity r g b)))
    (case *terminal-color-opt*
      (:dark (good-contrast-p luminosity +dark-luminosity+))
      (:light (good-contrast-p +light-luminosity+ luminosity)))))

(defun create-valid-colors ()
  (let ((colors nil)
        (labs nil))
    (flet ((distinct-lab-p (L1 a1 b1 L2 a2 b2)
             (>= (delta-e-76 L1 a1 b1 L2 a2 b2) 3.0)))
      (loop :for rgb :from 0 :to (expt 256 3) :by (floor (/ (expt 256 3) +color-count+))
            :do (let ((r (ldb (byte 8 16) rgb))
                      (g (ldb (byte 8 8) rgb))
                      (b (ldb (byte 8 0) rgb)))
                  (when (allowed-contrast-p r g b)
                    (multiple-value-bind (L1 a1 b1) (rgb-to-lab r g b)
                      (when (or (not labs)
                                (every #'(lambda (l) (distinct-lab-p L1 a1 b1 (first l) (second l) (third l))) labs))
                        (push (list r g b) colors)
                        (push (list L1 a1 b1) labs)))))))
    colors))

(defun hash-djb2 (string)
  (let ((ex (expt 2 64)))
    (reduce (lambda (hash c)
              (mod (+ (* 33 hash) c) ex))
            string
            :initial-value 5381
            :key #'char-code)))

(defun find-color (string)
  (or (gethash string *color-map*)
      (setf (gethash string *color-map*) (nth (mod (hash-djb2 string) (length *colors*)) *colors*))))

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

(defun run (paths)
  (when paths
    (let ((launch-args (append (list "tail" "-F") (if (listp paths) paths (list paths)))))
      (let* ((launch-info (uiop:launch-program launch-args :output :stream))
             (raw-input-stream (uiop:process-info-output launch-info))
             (input-stream (flexi-streams:make-flexi-stream raw-input-stream)))
        (setf (flexi-streams:flexi-stream-element-type input-stream) '(unsigned-byte 8)
              *colors* (create-valid-colors))
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

(defparameter *ui*
  (adopt:make-interface
   :name "tailf"
   :summary "Wraps the tail command line utility, specifically when tailing multiple files. Colorize the output from each file differently."
   :usage "[OPTIONS] FILE [FILE]+"
   :help "FILE can be any file glob acceptable to the tail command line utility."
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
                 (cond ((eql (gethash 'color options) 'dark)
                        (setf *terminal-color-opt* :dark))
                       ((eql (gethash 'color options) 'light)
                        (setf *terminal-color-opt* :light))
                       (t
                        (setf *terminal-color-opt* :dark)))
                 (run arguments)))
        (user-error (e) (adopt:print-error-and-exit e))))))
