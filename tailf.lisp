(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :flexi-streams :with-user-abort) :silent t))

(defpackage :tailf
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :tailf)

;;;; Configuration -----------------------------------------------

(defparameter *colors-for-dark-terminal* '((240 248 255)  ; alice-blue
                                           (245 245 220)  ; beige
                                           (255 228 196)  ; bisque
                                           (255 235 205) ; blanched-almond
                                           (255 255 255) ; bright-white
                                           (220 220 220) ; gainsboro
                                           (240 255 240) ; honeydew
                                           (255 255 240) ; ivory
                                           (255 240 245) ; lavender-blush
                                           (230 230 250) ; lavender
                                           (255 250 205) ; lemon-chiffon
                                           (240 128 128) ; light-coral
                                           (224 255 255) ; light-cyan
                                           (250 250 210) ; light-goldenrod-yellow
                                           (144 238 144) ; light-green
                                           (240 230 140) ; light-khaki
                                           (255 182 255) ; light-magenta
                                           (255 182 193) ; light-pink
                                           (255 160 122) ; light-salmon
                                           (135 206 250) ; light-sky-blue
                                           (176 196 222) ; light-steel-blue
                                           (245 255 250) ; mint-cream
                                           (255 228 225) ; misty-rose
                                           (173 216 230) ; pale-blue
                                           (238 232 170) ; pale-goldenrod
                                           (175 238 238) ; pale-turquoise
                                           (255 218 185) ; peach-puff
                                           (176 224 230) ; powder-blue
                                           (216 191 216) ; thistle
                                           (245 222 179) ; wheat
                                           ))

(defparameter *colors-for-light-terminal* '((165 42 42)  ; brown
                                            (54 69 79)   ; charcoal
                                            (123 63 0)   ; chocolate
                                            (220 20 60)  ; crimson
                                            (0 0 139)    ; dark-blue
                                            (0 139 139)  ; dark-cyan
                                            (184 134 11) ; dark-goldenrod
                                            (0 100 0)    ; dark-green
                                            (139 0 139) ; dark-magenta
                                            (85 107 47) ; dark-olive-green
                                            (153 50 204) ; dark-orchid
                                            (139 0 0)    ; dark-red
                                            (72 61 139) ; dark-slate-blue
                                            (47 79 79) ; dark-slate-gray
                                            (105 105 105) ; dim-gray
                                            (178 34 34)   ; firebrick
                                            (34 139 34) ; forest-green
                                            (42 52 57)  ; gunmetal
                                            (75 0 130)  ; indigo
                                            (0 0 0)     ; jet-black
                                            (128 0 0)   ; maroon
                                            (25 25 112) ; midnight-blue
                                            (0 73 83) ; midnight-green
                                            (0 0 128) ; navy-blue
                                            (107 142 35) ; olive-drab
                                            (128 128 0)  ; olive
                                            (0 33 71)    ; oxford-blue
                                            (102 51 153) ; rebecca-purple
                                            (139 69 19) ; saddle-brown
                                            (0 128 128) ; teal
                                            ))

(defvar *colors* (copy-list *colors-for-dark-terminal*)) ; default to dark
(defvar *start-color-pos* 0)
(defvar *color-map* (make-hash-table :test #'equalp))

;;;; Errors ------------------------------------------------------

(define-condition user-error (error) ())

;;;; Functionality -----------------------------------------------

(defun shuffle-colors ()
  (loop :for x :from (1- (length *colors*)) :downto 1
        :do (let ((i (random (1+ x))))
              (rotatef (nth x *colors*) (nth i *colors*)))))

(defun find-color (string)
  (or (gethash string *color-map*)
      (setf (gethash string *color-map*) (pop *colors*))))

(defun ansi-color-start (color)
  (destructuring-bind (r g b) color
    (format nil "~C[38;2;~D;~D;~Dm" #\Escape r g b)))

(defun ansi-color-end ()
  (format nil "~C[0m" #\Escape))

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
        (setf (flexi-streams:flexi-stream-element-type input-stream) '(unsigned-byte 8))
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
                        (setf *colors* (copy-list *colors-for-dark-terminal*)))
                       ((eql (gethash 'color options) 'light)
                        (setf *colors* (copy-list *colors-for-light-terminal*)))
                       (t
                        (setf *colors* (copy-list *colors-for-dark-terminal*))))
                 (run arguments)))
        (user-error (e) (adopt:print-error-and-exit e))))))
