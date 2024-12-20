(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :with-user-abort) :silent t))

(defpackage :wwf-wordle
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :wwf-wordle)

;;;; Configuration -----------------------------------------------
(defparameter +word-len+ 5)
(defparameter +word-path+ "~/Documents/wwf_wordle.txt")

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

;;;; Functionality -----------------------------------------------
(defun load-word-list ()
  "Loads the dictionary file.

Each line is assumed to be one word.  Each word must be
`+word-len+' in size (others are filtered out).  Each word
is uppercased."
  (with-open-file (word-file +word-path+)
    (loop for line = (read-line word-file nil)
          while line
          if (= (length line) +word-len+)
            collect (string-upcase line))))

;;; --------------------------------------------------------------

(defun random-word (word-list)
  "Select random word from list."
  (nth (random (length word-list)) word-list))

;;; --------------------------------------------------------------

(defun word-pfp (word &optional (seed 1))
  "Compute the Prime Factor Product for a word.

Each letter is assigned a prime number.  Lookup each
letter in the word and find its prime number, then multiply
them all together.

This function is a convenient way of collecting anagrams,
as anagrams will all have the same final PFP value."
  (let ((pfp seed))
    (flet ((char-pf (ch)
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
               (t 1))))
      (loop for c across word
            do (setf pfp (* pfp (char-pf c))))
      (max pfp 0))))

;;; --------------------------------------------------------------

(defun letter-cardinality (word)
  (let ((letter-hash (make-hash-table)))
    (loop for c across word
          do (setf (gethash c letter-hash) 1))
    (hash-table-count letter-hash)))

;;; --------------------------------------------------------------

(defun word-score (word)
  (* (word-pfp word) (expt 100 (- 6 (letter-cardinality word)))))

;;; --------------------------------------------------------------

(defun next-word (word-list)
  "Returns a random word with the lowest PFP value from WORD-LIST."
  (let ((result nil)
        (min-score (apply #'min (mapcar #'word-score word-list))))
    (loop for word in word-list
          do (when (= (word-score word) min-score)
               (push word result)))
    (random-word result)))

;;; --------------------------------------------------------------

(defun possible-char-p (ch exact-char anti-chars bad-chars)
  "Return t if CH satisfies constraints described by the rest of the arguments.

CH:             The character to test.
EXACT-CHAR:     The exact character CH is supposed to be or nil.
ANTI-CHARS:     List of uppercase characters that are invalid for the current
                position.
BAD-CHARS:      List of uppercase characters that invalid everywhere."
  (if exact-char
      (eql ch exact-char)
      (not (or (member ch anti-chars :test #'eql)
               (member ch bad-chars :test #'eql)))))

;;; --------------------------------------------------------------

(defun possible-word-p (word chars-in-word correct-chars anti-chars bad-chars)
  "Return t if WORD satisfies constraints described by the rest of the arguments.

WORD:           The word to test; will be same length as solution.
CHARS-IN-WORD:  List of uppercase characters that must be somewhere in the solution.
CORRECT-CHARS:  List of uppercase characters that are correct; position dependent;
                each element is either a character or nil; nil indicates
                no correct character identified for that position;
                length of list will match length of solution.
ANTI-CHARS:     List of lists; position dependent; each element is a list of
                uppercase characters representing characters that are definitely
                not at that position in the solution; length of ANTI-CHARS list
                will match length of solution.
BAD-CHARS:      List of uppercase characters that do not exist anywhere in
                the solution."
  (and (loop for i from 0 to (1- (length word))
             always (possible-char-p (char word i)
                                     (nth i correct-chars)
                                     (nth i anti-chars)
                                     bad-chars))
       (or (null chars-in-word)
           (loop for ch in chars-in-word
                 always (find ch word :test #'eql)))))

;;; --------------------------------------------------------------

(defun make-reply (guess solution)
  "If the code is playing itself, generate the 'reply' string that the user normally enters."
  (let ((reply nil))
    (when solution
      (setf reply (make-string +word-len+ :initial-element #\.))
      (dotimes (i (length guess))
        (cond ((eql (char guess i) (char-upcase (char solution i)))
               (setf (char reply i) (char guess i)))
              ((find (char guess i) (string-upcase solution) :test #'eql)
               (setf (char reply i) (char-downcase (char guess i))))))
      (format t "~A~%" reply)) ; Emit the value to the terminal so it looks like the user entered it
    reply))

;;;; Run ---------------------------------------------------------
(defun run (first-word solution)
  "Solve a Wordle puzzle in 'hard mode'.

If :FIRST-WORD is supplied, start with that word.  If not supplied, a random word from the
loaded dictionary will be selected.

If :SOLUTION is supplied, solve the puzzle without human interaction.  If not supplied,
the function will prompt the user for feedback on guesses.

If :SOLUTION is not supplied then the user must tell the function how each letter in its
guess relates to the solution.  For each letter, three options are possible:

. = guessed letter is not in the solution at all
<lowercase letter> = the letter is in the solution, but not at that position
<uppercase letter> = correct letter in the correct position

It follows that the reply must therefore be of the same length as the guess."
  (let ((word-list (load-word-list))
        (guess-count 1))
    (setf *random-state* (make-random-state t))
    (do ((guess (or (and first-word
                         (string-upcase first-word))
                    (random-word word-list)))
         (anti-chars (make-sequence 'list +word-len+ :initial-element nil)) ; list-of-lists-of-chars: chars that cannot be in a position
         (correct-chars (make-sequence 'list +word-len+ :initial-element nil)) ; list-of-chars: correct char for a position
         (chars-in-word nil)       ; chars we know must be in solution
         (bad-chars nil)           ; chars we know are not in solution
         (solvedp nil))
        (solvedp)
      (format t "~%")
      (format t "Dictionary Size: ~:D~%" (length word-list))
      (when (<= (length word-list) 20)
        (format t "Remaining: ~{~a~^, ~}~%" word-list))
      (format t "Guess #~D:~12T~A~%" guess-count guess)
      (format t "Reply:~12T")
      (finish-output nil)
      (let ((reply (or (make-reply guess solution)
                       (read-line)))
            (exact-chars-found 0))
        (cond ((string-equal reply "*")
               (setf word-list (remove-if
                                (lambda (w) (string-equal w guess))
                                word-list)
                     guess (next-word word-list)))
              ((/= (length reply) +word-len+)
               (format t "Reply must be exactly ~D characters long~%~%" +word-len+))
              (t
               (progn
                 (dotimes (i +word-len+)
                   (let ((ch (char reply i)))
                     (cond ((upper-case-p ch)
                            (setf (nth i correct-chars) ch)
                            (incf exact-chars-found))
                           ((lower-case-p ch)
                            (pushnew (char-upcase ch) (nth i anti-chars) :test #'eql)
                            (pushnew (char-upcase ch) chars-in-word :test #'eql))
                           (t
                            (pushnew (char guess i) bad-chars :test #'eql)))))
                 (if (= exact-chars-found +word-len+)
                     (setf solvedp t)
                     (setf word-list (remove-if-not
                                      (lambda (w) (possible-word-p w
                                                                   chars-in-word
                                                                   correct-chars
                                                                   anti-chars
                                                                   bad-chars))
                                      word-list)
                           guess (next-word word-list)
                           guess-count (1+ guess-count)))
                 (if (null word-list)
                     (error "No more words to choose from!")))))))
    guess-count))

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

(defparameter *first-word*
  (adopt:make-option 'first-word
                     :help "If supplied, start with this word. If not supplied, start with a random word."
                     :parameter "first-word"
                     :short #\f
                     :initial-value nil
                     :reduce #'adopt:last))

(defparameter *solution*
  (adopt:make-option 'solution
                     :help "If supplied, solve the puzzle without human interaction. If not supplied then the user must interactively tell the function how each letter in its guess relates to the solution.

. = guessed letter is not in the solution
<lowercase letter> = letter is in the solution but not at that position
<uppercase letter> = correct letter in the correct position

Also, the user can enter only a '*' to indicate that the guessed word is utterly invalid (i.e. not in the dictionary)."
                     :parameter "solution"
                     :short #\s
                     :initial-value nil
                     :reduce #'adopt:last))

(defparameter *ui*
  (adopt:make-interface
   :name "wwf-wordle"
   :summary "Solve Wordle-style puzzles using the dictionary from Words With Friends."
   :usage "[-f FIRST-WORD] [-s SOLUTION]"
   :help ""
   :contents (list *option-help*
                   *first-word*
                   *solution*)
   ))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      (declare (ignore arguments))
      (when (gethash 'help options nil)
        (adopt:print-help-and-exit *ui*))
      (handler-case (run (gethash 'first-word options nil) (gethash 'solution options nil))
        (user-error (e) (adopt:print-error-and-exit e))))))

