(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload '(:adopt :yason :alexandria) :silent t))


(defpackage :json-string

  (:use :cl)

  (:export :toplevel :*ui*))


(in-package :json-string)


;;;; Configuration ------------------------------------------------------------

(defparameter *version* "0.0.1")



;;;; Functionality ------------------------------------------------------------

(defun stringify (stream)

  (yason:encode

    (alexandria:read-stream-content-into-string stream)

    *standard-output*))



;;;; Run ----------------------------------------------------------------------

(defun run (paths)

  (if (null paths)

    (setf paths '("-")))

  (dolist (path paths)

    (fresh-line)

    (if (string= "-" path)

      (stringify *standard-input*)

      (with-open-file (stream path :direction :input)

        (stringify stream)))))



;;;; User Interface -----------------------------------------------------------

(defparameter *examples*

  '(("Stringify standard input:" . "echo 'foo and bar' | json-string")

    ("Turn 3 files into 3 strings:" . "json-string foo bar baz")

    ("Turn 3 files into one big string:" . "cat foo bar baz | json-string")))



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



(adopt:define-string *help-text*

  "json-string takes a stream of data (either standard input or a file) and ~

   renders its content into a JSON string, suitable for copying and pasting ~

   into a JSON file.  If multiple files are specified, they will each be ~

   converted into a string and printed one per line.")


(defparameter *ui*

  (adopt:make-interface

    :name "json-string"

    :usage "[OPTIONS] [FILE...]"

    :summary "render content as a JSON string"

    :help *help-text*

    :examples *examples*

    :contents (list *option-help*

                    *option-version*)))



(defmacro quit-on-ctrl-c (&body body)

  `(handler-case

     (progn ,@body)

     #+sbcl (sb-sys:interactive-interrupt (c)

               (declare (ignore c))

               (adopt:exit))))



(defun parse-options-or-exit (ui)

  (handler-case (adopt:parse-options ui)

    (error (c) (adopt:print-error-and-exit c))))



(defun toplevel (&aux arguments options)

  #+sbcl (sb-ext:disable-debugger)

  (quit-on-ctrl-c

    (setf (values arguments options)

          (parse-options-or-exit *ui*))

    (handler-case

        (progn

          (when (gethash 'help options)

            (adopt:print-help-and-exit *ui*))

          (when (gethash 'version options)

            (write-line *version*)

            (adopt:exit))

          (run arguments))

      (error (c) (adopt:print-error-and-exit c)))))

