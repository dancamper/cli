(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :flexi-streams :with-user-abort) :silent t))

(defpackage :tailf
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :tailf)

;;;; Configuration -----------------------------------------------

(defparameter *colors-for-dark-terminal* '((255 255 255) ; Bright White
                                           (250 250 210) ; Light Goldenrod Yellow
                                           (224 255 255) ; Light Cyan
                                           (173 216 230) ; Pale Blue
                                           (144 238 144) ; Light Green
                                           (230 230 250) ; Lavender
                                           (255 182 193) ; Light Pink
                                           (255 218 185) ; Peach Puff
                                           (240 128 128) ; Light Coral
                                           (255 250 205) ; Lemon Chiffon
                                           (255 228 225) ; Misty Rose
                                           (255 160 122) ; Light Salmon
                                           (175 238 238) ; Pale Turquoise
                                           (216 191 216) ; Thistle
                                           (176 224 230) ; Powder Blue
                                           (135 206 250) ; Light Sky Blue
                                           (176 196 222) ; Light Steel Blue
                                           (240 255 240) ; Honeydew
                                           (245 255 250) ; Mint Cream
                                           (240 248 255) ; Alice Blue
                                           (255 235 205) ; Blanched Almond
                                           (255 228 196) ; Bisque
                                           (255 240 245) ; Lavender Blush
                                           (238 232 170) ; Pale Goldenrod
                                           (245 245 220) ; Beige
                                           (255 255 240) ; Ivory
                                           (245 222 179) ; Wheat
                                           (220 220 220) ; Gainsboro
                                           (255 182 255) ; Light Magenta
                                           (240 230 140) ; Light Khaki
                                           (255 245 238) ; Seashell
                                           (255 250 250) ; Snow
                                           (255 239 213) ; Papaya Whip
                                           (200 200 255) ; Periwinkle
                                           (219 112 147) ; Pale Violet Red
                                           (250 240 230) ; Linen
                                           (255 250 240) ; Floral White
                                           (253 245 230) ; Old Lace
                                           (247 231 206) ; Champagne
                                           (252 230 201) ; Eggshell
                                           (255 248 220) ; Cornsilk
                                           (240 255 255) ; Azure
                                           (248 248 255) ; Ghost White
                                           (245 245 245) ; White Smoke
                                           (224 208 255) ; Mauve
                                           (137 207 240) ; Baby Blue
                                           (255 255 224) ; Light Yellow
                                           (255 253 208) ; Cream
                                           (255 244 214) ; Buttermilk
                                           (248 202 204) ; Rose Quartz
                                           ))

(defparameter *colors-for-light-terminal* '((0 0 0) ; Jet Black
                                            (54 69 79) ; Charcoal
                                            (47 79 79) ; Dark Slate Gray
                                            (0 0 128)  ; Navy Blue
                                            (25 25 112) ; Midnight Blue
                                            (75 0 130)  ; Indigo
                                            (85 107 47) ; Dark Olive Green
                                            (34 139 34) ; Forest Green
                                            (0 100 0)   ; Dark Green
                                            (0 139 139) ; Dark Cyan
                                            (72 61 139) ; Dark Slate Blue
                                            (139 0 139) ; Dark Magenta
                                            (128 0 0)   ; Maroon
                                            (139 0 0)   ; Dark Red
                                            (178 34 34) ; Firebrick
                                            (139 69 19) ; Saddle Brown
                                            (123 63 0)  ; Chocolate
                                            (184 134 11) ; Dark Goldenrod
                                            (128 128 0)  ; Olive
                                            (105 105 105) ; Dim Gray
                                            (153 50 204) ; Dark Orchid
                                            (102 51 153) ; Rebecca Purple
                                            (165 42 42)  ; Brown
                                            (0 0 139)    ; Dark Blue
                                            (220 20 60)  ; Crimson
                                            (0 128 128)  ; Teal
                                            (107 142 35) ; Olive Drab
                                            (42 52 57)   ; Gunmetal
                                            (0 73 83) ; Midnight Green
                                            (0 33 71) ; Oxford Blue
                                            (50 47 46) ; Ebony
                                            (41 36 33) ; Raven
                                            (0 51 102) ; Deep Sea Blue
                                            (15 82 186) ; Sapphire
                                            (43 0 28)   ; Aubergine
                                            (53 94 59)  ; Hunter Green
                                            (0 106 78)  ; Bottle Green
                                            (0 102 102) ; Dark Teal
                                            (128 0 32)  ; Deep Maroon
                                            (114 47 55) ; Cordovan
                                            (115 74 18) ; Dark Copper
                                            (40 30 20) ; Midnight Brown
                                            (0 49 83)  ; Prussian Blue
                                            (74 100 108) ; Deep Space Sparkle
                                            (65 72 51)   ; Rifle Green
                                            (102 0 0)    ; OxBlood
                                            (0 71 171)   ; Cobalt Blue
                                            (0 77 77)    ; Deep Teal
                                            (50 0 50)    ; Blackberry
                                            (0 31 63)    ; Ink Blue
                                            ))

(defvar *colors* (copy-list *colors-for-dark-terminal*)) ; default to dark
(defvar *start-color-pos* 0)
(defvar *color-map* (make-hash-table :test #'equalp))
(defvar *my-random-state* nil)

;;;; Errors ------------------------------------------------------

(define-condition user-error (error) ())

;;;; Functionality -----------------------------------------------

(defun shuffle-colors ()
  (loop :for x :from (1- (length *colors*)) :downto 1
        :do (let ((i (random (1+ x) *my-random-state*)))
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
        (setf (flexi-streams:flexi-stream-element-type input-stream) '(unsigned-byte 8)
              *my-random-state* (make-random-state t))
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
