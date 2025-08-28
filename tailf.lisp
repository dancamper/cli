(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :alexandria :flexi-streams :random-state :with-user-abort) :silent t))

(defpackage :tailf
  (:use :cl)
  (:local-nicknames (#:a #:alexandria))
  (:export :toplevel *ui*))

(in-package :tailf)

;;;; Configuration -----------------------------------------------

(a:define-constant +colors-for-dark-terminal+ '("#696969" ; dimgray
                                                "#808080" ; gray
                                                "#a9a9a9" ; darkgray
                                                "#d3d3d3" ; lightgray
                                                "#ffffff" ; white
                                                "#2f4f4f" ; darkslategray
                                                "#556b2f" ; darkolivegreen
                                                "#8b4513" ; saddlebrown
                                                "#6b8e23" ; olivedrab
                                                "#a52a2a" ; brown
                                                "#2e8b57" ; seagreen
                                                "#7f0000" ; maroon2
                                                "#191970" ; midnightblue
                                                "#808000" ; olive
                                                "#483d8b" ; darkslateblue
                                                "#b22222" ; firebrick
                                                "#5f9ea0" ; cadetblue
                                                "#778899" ; lightslategray
                                                "#008000" ; green
                                                "#3cb371" ; mediumseagreen
                                                "#bc8f8f" ; rosybrown
                                                "#663399" ; rebeccapurple
                                                "#008080" ; teal
                                                "#b8860b" ; darkgoldenrod
                                                "#bdb76b" ; darkkhaki
                                                "#cd853f" ; peru
                                                "#4682b4" ; steelblue
                                                "#000080" ; navy
                                                "#d2691e" ; chocolate
                                                "#9acd32" ; yellowgreen
                                                "#20b2aa" ; lightseagreen
                                                "#cd5c5c" ; indianred
                                                "#4b0082" ; indigo
                                                "#32cd32" ; limegreen
                                                "#daa520" ; goldenrod
                                                "#8fbc8f" ; darkseagreen
                                                "#800080" ; purple
                                                "#b03060" ; maroon3
                                                "#d2b48c" ; tan
                                                "#66cdaa" ; mediumaquamarine
                                                "#9932cc" ; darkorchid
                                                "#ff0000" ; red
                                                "#ff4500" ; orangered
                                                "#00ced1" ; darkturquoise
                                                "#ff8c00" ; darkorange
                                                "#ffa500" ; orange
                                                "#ffd700" ; gold
                                                "#6a5acd" ; slateblue
                                                "#ffff00" ; yellow
                                                "#c71585" ; mediumvioletred
                                                "#0000cd" ; mediumblue
                                                "#40e0d0" ; turquoise
                                                "#7fff00" ; chartreuse
                                                "#00ff00" ; lime
                                                "#9400d3" ; darkviolet
                                                "#ba55d3" ; mediumorchid
                                                "#00fa9a" ; mediumspringgreen
                                                "#00ff7f" ; springgreen
                                                "#4169e1" ; royalblue
                                                "#e9967a" ; darksalmon
                                                "#dc143c" ; crimson
                                                "#00ffff" ; aqua
                                                "#00bfff" ; deepskyblue
                                                "#f4a460" ; sandybrown
                                                "#9370db" ; mediumpurple
                                                "#0000ff" ; blue
                                                "#a020f0" ; purple3
                                                "#f08080" ; lightcoral
                                                "#adff2f" ; greenyellow
                                                "#ff6347" ; tomato
                                                "#da70d6" ; orchid
                                                "#d8bfd8" ; thistle
                                                "#b0c4de" ; lightsteelblue
                                                "#ff7f50" ; coral
                                                "#ff00ff" ; fuchsia
                                                "#1e90ff" ; dodgerblue
                                                "#db7093" ; palevioletred
                                                "#f0e68c" ; khaki
                                                "#fa8072" ; salmon
                                                "#eee8aa" ; palegoldenrod
                                                "#ffff54" ; laserlemon
                                                "#6495ed" ; cornflower
                                                "#dda0dd" ; plum
                                                "#90ee90" ; lightgreen
                                                "#add8e6" ; lightblue
                                                "#87ceeb" ; skyblue
                                                "#ff1493" ; deeppink
                                                "#7b68ee" ; mediumslateblue
                                                "#ffa07a" ; lightsalmon
                                                "#afeeee" ; paleturquoise
                                                "#7fffd4" ; aquamarine
                                                "#ffdead" ; navajowhite
                                                "#ff69b4" ; hotpink
                                                "#ffe4c4" ; bisque
                                                "#e6e6fa" ; lavender
                                                "#ffe4e1" ; mistyrose
                                                "#fff8dc" ; cornsilk
                                                "#f0fff0" ; honeydew
                                                "#e0ffff" ; lightcyan
                                                "#ffb6c1" ; lightpink
                                                ) :test #'equalp)

(a:define-constant +colors-for-light-terminal+ '("#000000" ; black
                                                 "#696969" ; dimgray
                                                 "#808080" ; gray
                                                 "#a9a9a9" ; darkgray
                                                 "#c0c0c0" ; silver
                                                 "#2f4f4f" ; darkslategray
                                                 "#556b2f" ; darkolivegreen
                                                 "#8b4513" ; saddlebrown
                                                 "#6b8e23" ; olivedrab
                                                 "#a0522d" ; sienna
                                                 "#a52a2a" ; brown
                                                 "#2e8b57" ; seagreen
                                                 "#228b22" ; forestgreen
                                                 "#191970" ; midnightblue
                                                 "#006400" ; darkgreen
                                                 "#708090" ; slategray
                                                 "#8b0000" ; darkred
                                                 "#808000" ; olive
                                                 "#483d8b" ; darkslateblue
                                                 "#b22222" ; firebrick
                                                 "#5f9ea0" ; cadetblue
                                                 "#008000" ; green
                                                 "#3cb371" ; mediumseagreen
                                                 "#bc8f8f" ; rosybrown
                                                 "#663399" ; rebeccapurple
                                                 "#008080" ; teal
                                                 "#b8860b" ; darkgoldenrod
                                                 "#bdb76b" ; darkkhaki
                                                 "#cd853f" ; peru
                                                 "#4682b4" ; steelblue
                                                 "#000080" ; navy
                                                 "#d2691e" ; chocolate
                                                 "#9acd32" ; yellowgreen
                                                 "#20b2aa" ; lightseagreen
                                                 "#cd5c5c" ; indianred
                                                 "#4b0082" ; indigo
                                                 "#32cd32" ; limegreen
                                                 "#daa520" ; goldenrod
                                                 "#7f007f" ; purple2
                                                 "#8fbc8f" ; darkseagreen
                                                 "#8b008b" ; darkmagenta
                                                 "#b03060" ; maroon3
                                                 "#d2b48c" ; tan
                                                 "#48d1cc" ; mediumturquoise
                                                 "#66cdaa" ; mediumaquamarine
                                                 "#9932cc" ; darkorchid
                                                 "#ff0000" ; red
                                                 "#ff4500" ; orangered
                                                 "#00ced1" ; darkturquoise
                                                 "#ff8c00" ; darkorange
                                                 "#ffa500" ; orange
                                                 "#ffd700" ; gold
                                                 "#6a5acd" ; slateblue
                                                 "#ffff00" ; yellow
                                                 "#c71585" ; mediumvioletred
                                                 "#0000cd" ; mediumblue
                                                 "#deb887" ; burlywood
                                                 "#40e0d0" ; turquoise
                                                 "#7fff00" ; chartreuse
                                                 "#00ff00" ; lime
                                                 "#9400d3" ; darkviolet
                                                 "#ba55d3" ; mediumorchid
                                                 "#00fa9a" ; mediumspringgreen
                                                 "#8a2be2" ; blueviolet
                                                 "#00ff7f" ; springgreen
                                                 "#4169e1" ; royalblue
                                                 "#e9967a" ; darksalmon
                                                 "#dc143c" ; crimson
                                                 "#00ffff" ; aqua
                                                 "#00bfff" ; deepskyblue
                                                 "#f4a460" ; sandybrown
                                                 "#9370db" ; mediumpurple
                                                 "#0000ff" ; blue
                                                 "#a020f0" ; purple3
                                                 "#f08080" ; lightcoral
                                                 "#adff2f" ; greenyellow
                                                 "#ff6347" ; tomato
                                                 "#da70d6" ; orchid
                                                 "#d8bfd8" ; thistle
                                                 "#b0c4de" ; lightsteelblue
                                                 "#ff7f50" ; coral
                                                 "#ff00ff" ; fuchsia
                                                 "#1e90ff" ; dodgerblue
                                                 "#db7093" ; palevioletred
                                                 "#f0e68c" ; khaki
                                                 "#fa8072" ; salmon
                                                 "#eee8aa" ; palegoldenrod
                                                 "#ffff54" ; laserlemon
                                                 "#6495ed" ; cornflower
                                                 "#dda0dd" ; plum
                                                 "#b0e0e6" ; powderblue
                                                 "#87ceeb" ; skyblue
                                                 "#ff1493" ; deeppink
                                                 "#7b68ee" ; mediumslateblue
                                                 "#ffa07a" ; lightsalmon
                                                 "#ee82ee" ; violet
                                                 "#98fb98" ; palegreen
                                                 "#87cefa" ; lightskyblue
                                                 "#7fffd4" ; aquamarine
                                                 "#ff69b4" ; hotpink
                                                 ) :test #'equalp)

(defvar *terminal-color-opt* :dark)
(defvar *colors* nil)
(defvar *color-map* (make-hash-table :test #'equalp))
(defvar *my-random-state* nil)

;;;; Errors ------------------------------------------------------

(define-condition user-error (error) ())

;;;; Functionality -----------------------------------------------

(defun assign-colors (hex-colors)
  (setf *colors* nil)
  (loop :for hex-color :in hex-colors
        :do (let* ((clean (if (char= (char hex-color 0) #\#)
                              (subseq hex-color 1)
                              hex-color)))
              (push (list (parse-integer (subseq clean 0 2) :radix 16)
                          (parse-integer (subseq clean 2 4) :radix 16)
                          (parse-integer (subseq clean 4 6) :radix 16))
                    *colors*)))
  *colors*)

(defun shuffle-colors ()
  (loop :for x :from (1- (length *colors*)) :downto 1
        :do (let ((i (random-state:random-int *my-random-state* 0 (1+ x))))
              (rotatef (nth x *colors*) (nth i *colors*)))))

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

(defun run (paths)
  (when paths
    (let ((launch-args (append (list "tail" "-F") (if (listp paths) paths (list paths)))))
      (let* ((launch-info (uiop:launch-program launch-args :output :stream))
             (raw-input-stream (uiop:process-info-output launch-info))
             (input-stream (flexi-streams:make-flexi-stream raw-input-stream)))
        (setf (flexi-streams:flexi-stream-element-type input-stream) '(unsigned-byte 8)
              *my-random-state* (random-state:make-generator :mersenne-twister-32 (get-universal-time)))
        (case *terminal-color-opt*
          ((:dark)
           (assign-colors +colors-for-dark-terminal+))
          ((:light)
           (assign-colors +colors-for-light-terminal+))
          (otherwise
           (assign-colors +colors-for-dark-terminal+)))
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
                        (setf *terminal-color-opt* :dark))
                       ((eql (gethash 'color options) 'light)
                        (setf *terminal-color-opt* :light))
                       (t
                        (setf *terminal-color-opt* :dark)))
                 (run arguments)))
        (user-error (e) (adopt:print-error-and-exit e))))))
