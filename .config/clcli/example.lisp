(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload '(:adopt) :silent t))


(defpackage :example

  (:use :cl)

  (:export :toplevel :*ui*))


(in-package :example)


;;;; Config -------------------------------------------------------------------

(defparameter *default-name* "World")



;;;; Functionality ------------------------------------------------------------

(defun run (name)

  (format t "Hello, ~A~%" name))



;;;; CLI ----------------------------------------------------------------------

(defparameter *help*

  (adopt:make-option 'help

    :help "display help and exit"

    :long "help"

    :short #\h

    :reduce (constantly t)))


(defparameter *name*

  (adopt:make-option 'name

    :help (format nil "say hello to NAME (default ~A)" *default-name*)

    :long "name"

    :short #\n

    :parameter "NAME"

    :initial-value *default-name*

    :reduce #'adopt:last))


(defparameter *ui*

  (adopt:make-interface

    :name "example"

    :usage "[-n NAME]"

    :summary "Say Hello."

    :help "An example program to show how to make well-behaved CLI tools in Common Lisp."

    :examples '(("Say hello:" . "example")

                ("Say hello to Alice:" . "example --name Alice"))

    :contents (list *help* *name*)))



(defun toplevel ()

  (handler-case

      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)

        (when (gethash 'help options)

          (adopt:print-help-and-exit *ui*))

        (unless (null arguments)

          (error "Unrecognized command-line arguments: ~S" arguments))

        (run (gethash 'name options)))

    (error (c) (adopt:print-error-and-exit c))))
