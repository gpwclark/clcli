(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload '(:adopt) :silent t))


(defpackage :subex

  (:use :cl)

  (:export :toplevel *ui*))


(in-package :subex)


;;;; Global Options and UI ----------------------------------------------------

(defparameter *o/help*

  (adopt:make-option 'help :long "help" :help "display help and exit" :reduce (constantly t)))


(defparameter *o/version*

  (adopt:make-option 'version :long "version" :help "display version and exit" :reduce (constantly t)))


(defparameter *ui/main*

  (adopt:make-interface

    :name "subex"

    :usage "[subcommand] [options]"

    :help "subcommand example program"

    :summary "an example program that uses subcommands"

    :contents (list *o/help* *o/version*)))


(defparameter *ui* *ui/main*)



;;;; Subcommand Foo -----------------------------------------------------------

(defparameter *o/foo/a*

  (adopt:make-option 'a :result-key 'mode :short #\a :help "run foo in mode A" :reduce (constantly :a)))


(defparameter *o/foo/b*

  (adopt:make-option 'b :result-key 'mode :short #\b :help "run foo in mode B" :reduce (constantly :b)))


(defparameter *ui/foo*

  (adopt:make-interface

    :name "subex foo"

    :usage "foo [-a|-b]"

    :summary "foo some things"

    :help "foo some things"

    :contents (list *o/foo/a* *o/foo/b*)))


(defun run/foo (mode)

  (format t "Running foo in ~A mode.~%" mode))



;;;; Subcommand Bar -----------------------------------------------------------

(defparameter *o/bar/meow*

  (adopt:make-option 'meow :long "meow" :help "meow loudly after each step" :reduce (constantly t)))


(defparameter *ui/bar*

  (adopt:make-interface

    :name "subex bar"

    :usage "bar [--meow] FILE..."

    :summary "bar some files"

    :help "bar some files"

    :contents (list *o/bar/meow*)))


(defun run/bar (paths meow?)

  (dolist (p paths)

    (format t "Bar-ing ~A.~%" p)

    (when meow?

      (write-line "meow."))))



;;;; Toplevel -----------------------------------------------------------------

(defun toplevel/foo (args)

  (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui/foo* args)

    (unless (null arguments)

      (error "Foo does not take arguments, got ~S" arguments))

    (run/foo (gethash 'mode options))))


(defun toplevel/bar (args)

  (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui/bar* args)

    (when (null arguments)

      (error "Bar requires arguments, got none."))

    (run/bar arguments (gethash 'meow options))))


(defun lookup-subcommand (string)

  (cond

    ((null string) (values nil *ui/main*))

    ((string= string "foo") (values #'toplevel/foo *ui/foo*))

    ((string= string "bar") (values #'toplevel/bar *ui/bar*))

    (t (error "Unknown subcommand ~S" string))))


(defun toplevel ()

  (sb-ext:disable-debugger)

  (multiple-value-bind (arguments global-options)

      (handler-bind ((adopt:unrecognized-option 'adopt:treat-as-argument))

        (adopt:parse-options *ui/main*))

    (when (gethash 'version global-options)

      (write-line "1.0.0")

      (adopt:exit))

    (multiple-value-bind (subtoplevel ui) (lookup-subcommand (first arguments))

      (when (or (null subtoplevel)

                (gethash 'help global-options))

        (adopt:print-help-and-exit ui))

      (funcall subtoplevel (rest arguments)))))

