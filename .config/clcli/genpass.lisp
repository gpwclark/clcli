(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload '(:adopt :series :iterate :str) :silent t))


(defpackage :genpass

  (:use :cl :iterate)

  (:export :toplevel :*ui*))


(in-package :genpass)


;;;; Configuration ------------------------------------------------------------

(defparameter *passphrase-sep* " ")

(defparameter *words* nil)

(defparameter *version* "0.0.1")

(defparameter *random-path* "/dev/urandom")

(defparameter *wordlist-path* "/usr/share/dict/scowl/english-words.20")

(defparameter *wordlist-encoding* :ISO-8859-1)



;;;; Functionality ------------------------------------------------------------

(defmacro -<> (value &body forms)

  (reduce (lambda (val expression)

            (subst val '<> expression))

          forms

          :initial-value value))


(defun safep (char)

  (or (char<= #\a char #\z)

      (char<= #\A char #\Z)

      (char<= #\0 char #\9)))


(defun word-list ()

  (unless *words*

    (setf *words*

          (with-open-file (s *wordlist-path* :external-format *wordlist-encoding*)

            (iterate

              (for line :in-stream s :using #'read-line)

              (when (every #'safep line)

                (collect line :result-type vector))))))

  *words*)


(defun random-unsigned-byte-32 ()

  (with-open-file (urandom *random-path* :element-type '(unsigned-byte 8))

    (logior (ash (read-byte urandom) 0)

            (ash (read-byte urandom) 8)

            (ash (read-byte urandom) 16)

            (ash (read-byte urandom) 24))))


(defun urandom (limit)

  (check-type limit (integer 0 (#.(expt 2 32))))

  (iterate

    (with threshold = (mod (expt 2 32) limit))

    (for candidate = (random-unsigned-byte-32))

    (finding (mod candidate limit) :such-that (>= candidate threshold))))


(defun random-elt (sequence)

  (elt sequence (urandom (length sequence))))


(defun random-words (n)

  (iterate

    (with words = (word-list))

    (repeat n)

    (collect (random-elt words))))


(defun random-sentence% (words)

  (str:join *passphrase-sep* (mapcar #'string-capitalize (random-words words))))


(defun random-sentence (words length)

  (iterate

    (for candidate = (random-sentence% words))

    (finding candidate :such-that (<= (length candidate) length))))


(defun random-garbage (length)

  (with-open-file (urandom *random-path* :element-type '(unsigned-byte 8))

    (-<> urandom

      (series:scan-stream <> #'read-byte)

      (series:map-fn t #'code-char <>)

      (series:choose-if #'safep <>)

      (series:subseries <> 0 length)

      (series:collect 'string <>))))


(defun random-smart (words length)

  (check-type words (integer 1))

  (format nil "~A~A~D"

          (random-sentence words (- length 1 (length *passphrase-sep*)))

          *passphrase-sep*

          (urandom 10)))



;;;; Run ----------------------------------------------------------------------

(defun run (length words smart smush?)

  (let ((*passphrase-sep* (if smush? "" " ")))

    (write-string

      (cond

        ((zerop words) (random-garbage length))

        (smart (random-smart words length))

        (t (random-sentence words length))))))



;;;; User Interface -----------------------------------------------------------

(defparameter *examples*

  '(("Generate a random passphrase no longer than 24 characters:"

     . "genpass --length 24")

    ("Generate a more random, harder to type password:"

     . "genpass --no-words")

    ("Generate a six word passphrase with no spaces:"

     . "genpass --words 6 --smush")))



(defparameter *option-help*

  (adopt:make-option 'help

    :help "Display help and exit."

    :long "help"

    :short #\h

    :reduce (constantly t)))


(defparameter *option-version*

  (adopt:make-option 'version

    :help "Display version information and exit."

    :long "version"

    :reduce (constantly t)))


(defparameter *option-random-path*

  (adopt:make-option 'random-path

    :help (format nil "Path to the source of randomness (default ~A)." *random-path*)

    :long "random-path"

    :parameter "PATH"

    :initial-value *random-path*

    :reduce #'adopt:last))


(defparameter *option-wordlist-path*

  (adopt:make-option 'wordlist-path

    :help (format nil "Path to the list of words (default ~A)." *wordlist-path*)

    :long "words-path"

    :parameter "PATH"

    :initial-value *wordlist-path*

    :reduce #'adopt:last))


(defparameter *option-length*

  (adopt:make-option 'length

    :help "Ensure password is no longer than N characters (default 40)."

    :long "length"

    :short #\l

    :parameter "N"

    :initial-value 40

    :reduce #'adopt:last

    :key #'parse-integer))


(defparameter *option-words*

  (adopt:make-option 'words

    :help "If non-zero, generate passphrases of N words instead of opaque strings (default 4)."

    :long "words"

    :short #\w

    :parameter "N"

    :initial-value 4

    :reduce #'adopt:last

    :key #'parse-integer))


(defparameter *option-no-words*

  (adopt:make-option 'no-words

    :result-key 'words

    :help "Shorthand for --words=0."

    :long "no-words"

    :short #\W

    :reduce (constantly 0)))


(adopt:defparameters (*option-smart* *option-no-smart*)

  (adopt:make-boolean-options 'smart

    :help "Smart mode (the default).  Generate as with --words, but add a number on the end to satisfy the red tape on many sites."

    :help-no "Turn off smart mode."

    :long "smart"

    :short #\s

    :initial-value t))


(adopt:defparameters (*option-smush* *option-no-smush*)

  (adopt:make-boolean-options 'smush

    :help "Don't include spaces in passphrases."

    :help-no "Include spaces in passphrases (the default)."

    :long "smush"

    :short #\m))


(defparameter *group-password-options*

  (adopt:make-group 'password-options

    :title "Password Options"

    :help "The format of generated passwords can be customized in a number of ways."

    :options (list *option-length*

                   *option-words* *option-no-words*

                   *option-smart* *option-no-smart*

                   *option-smush* *option-no-smush*)))


(defparameter *group-data-sources*

  (adopt:make-group 'data-source-options

    :title "Data Sources"

    :options (list *option-random-path*

                   *option-wordlist-path*)))



(adopt:define-string *help-text*

  "This utility generates random passwords for use with a password ~

   manager like password-store.~@

   ~@

   By default genpass will generate long passphrases of several random ~

   words.  This is done so that they're not absolutely miserable to type ~

   on the rare occasions that you're trying to log in on someone else's ~

   machine by reading the password off of your phone.")


(defparameter *ui*

  (adopt:make-interface

    :name "genpass"

    :usage "[OPTIONS]"

    :summary "generate a random password"

    :help *help-text*

    :examples *examples*

    :contents (list *option-help*

                    *option-version*

                    *group-password-options*

                    *group-data-sources*)))



(defun toplevel ()

  (handler-case

      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)

        (when (gethash 'help options)

          (adopt:print-help-and-exit *ui*))

        (when (gethash 'version options)

          (write-line *version*)

          (adopt:exit))

        (when arguments

          (error "Unrecognized command line arguments: ~S" arguments))

        (let ((*wordlist-path* (gethash 'wordlist-path options))

              (*random-path* (gethash 'random-path options)))

          (run (gethash 'length options)

               (gethash 'words options)

               (gethash 'smart options)

               (gethash 'smush options))))

    (error (c) (adopt:print-error-and-exit c))))

