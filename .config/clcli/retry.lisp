(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload

    '(:adopt :iterate :external-program :parse-number :alexandria :with-user-abort)

    :silent t))


(defpackage :retry

  (:use :cl :iterate)

  (:export :toplevel :*ui*))


(in-package :retry)


;;;; Configuration ------------------------------------------------------------

(defparameter *version* "0.0.1")



;;;; Functionality ------------------------------------------------------------

(defun backoff-immediate ()

  (lambda (try) (declare (ignore try))))


(defun backoff-constant (seconds)

  (lambda (try)

    (declare (ignore try))

    (sleep seconds)))


(defun backoff-exponential (seconds)

  (lambda (try)

    (sleep (* try seconds))))


(defun retry (command &key backoff tries)

  (iterate

    (with (program . args) = command)

    (for try :from 1)

    (for (values nil code) =

         (external-program:run program args :input t :output t :error t))

    (finding code :such-that #'zerop :on-failure 1)

    (while (or (null tries) (< try tries)))

    (when backoff

      (funcall backoff try))))



;;;; Run ----------------------------------------------------------------------

(defun run (arguments options)

  (retry (let ((shell (gethash 'shell options)))

           (if shell

             `("sh" "-c" ,shell)

             arguments))

         :tries (gethash 'tries options)

         :backoff (gethash 'backoff options)))



;;;; User Interface -----------------------------------------------------------

(adopt:define-string *help*

  "retry runs another command, retrying it if it returns a non-zero exit code.~@

   ~@

   Options are available for configuring the number of retries and how long ~

   to wait before retrying.  If the command eventually succeeds, retry will ~

   return an exit code of 0, otherwise it will return an exit code of 1.")


(adopt:define-string *help-command*

  "By default, any non-option arguments to retry will designate the command ~

   to be run.  If the command itself has options, you'll need to use -- to tell ~

   retry which options belong to the command, e.g.:~@

   ~@

   ~:

    retry --backoff 5 -- ping -c 1 -w 1 example.com~@

   ~@

   You can use the --shell option to specify the command to run as a single ~

   string, which can be handy if you need to redirect its output:~@

   ~@

   ~:

    retry -t5 -b1 --shell 'curl -Lsf stevelosh.com | grep -q Hello'")


(defparameter *examples*

  '(("Retry flashing a cheap EEPROM a few times before giving up:"

     . "retry --tries 3 -- minipro -p AT28C256 -w rom.bin")

    ("Try to curl a healthcheck endpoint with exponential backoff:"

     . "retry --backoff 1 --tries 5 --shell \"curl -Lsf 'http://example.com/healthcheck/' >/dev/null\"")))



(defparameter *option-shell*

  (adopt:make-option 'shell

    :help "Execute `sh -c \"$CMD\"`, instead of passing the command as separate arguments to retry."

    :long "shell"

    :short #\s

    :parameter "CMD"

    :reduce #'adopt:last))


(defparameter *option-no-shell*

  (adopt:make-option 'no-shell

    :result-key 'shell

    :help "Execute the non-option arguments to retry as the command (the default)."

    :long "no-shell"

    :short #\S

    :reduce (constantly nil)))


(defparameter *option-immediate*

  (adopt:make-option 'immediate

    :result-key 'backoff

    :help "Retry the command immediately on failure (the default)."

    :long "immediate"

    :short #\i

    :reduce (constantly (backoff-immediate))))


(defparameter *option-wait*

  (adopt:make-option 'wait

    :result-key 'backoff

    :help "Wait N seconds before retrying a failed command."

    :long "wait"

    :short #\w

    :parameter "N"

    :key (alexandria:compose #'backoff-constant #'parse-number:parse-number)

    :reduce #'adopt:last))


(defparameter *option-backoff*

  (adopt:make-option 'backoff

    :result-key 'backoff

    :help "Backoff (N * try) seconds before retrying a failed command."

    :long "backoff"

    :short #\b

    :parameter "N"

    :key (alexandria:compose #'backoff-exponential #'parse-number:parse-number)

    :reduce #'adopt:last))


(defparameter *option-tries*

  (adopt:make-option 'tries

    :help "Maximum number of tries to attempt."

    :long "tries"

    :short #\t

    :parameter "N"

    :initial-value nil

    :key #'parse-integer

    :reduce #'adopt:last))


(defparameter *option-try-forever*

  (adopt:make-option 'try-forever

    :result-key 'tries

    :help "Try forever (the default)."

    :long "try-forever"

    :short #\T

    :reduce (constantly nil)))


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



(defparameter *ui*

  (adopt:make-interface

    :name "retry"

    :usage "[OPTIONS] -- COMMAND [ARG...]"

    :summary "retry running a command"

    :help *help*

    :examples *examples*

    :contents (list

                (adopt:make-group

                  'tries

                  :title "Number of Tries"

                  :options (list

                             *option-tries*

                             *option-try-forever*))


                (adopt:make-group

                  'backoff-policy

                  :title "Backoff Policy"

                  :options (list

                             *option-immediate*

                             *option-wait*

                             *option-backoff*))


                (adopt:make-group

                  'command

                  :title "Specifying a Command"

                  :help *help-command*

                  :options (list

                             *option-shell*

                             *option-no-shell*))


                *option-help*

                *option-version*)))




(defun parse-options-or-exit (ui)

  (handler-case (adopt:parse-options ui)

    (error (c) (adopt:print-error-and-exit c))))



(defun toplevel ()

  #+sbcl (sb-ext:disable-debugger)

  (handler-case

      (with-user-abort:with-user-abort

        (multiple-value-bind (arguments options)

            (parse-options-or-exit *ui*)

          (cond

            ((gethash 'help options) (adopt:print-help-and-exit *ui*))

            ((gethash 'version options) (write-line *version*) (adopt:exit))

            (t (adopt:exit (run arguments options))))))

    (with-user-abort:user-abort () (adopt:exit 130))))

