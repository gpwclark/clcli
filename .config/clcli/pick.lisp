(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload '(:adopt :with-user-abort) :silent t))


(defpackage :pick

  (:use :cl)

  (:export :toplevel :*ui*))


(in-package :pick)


;;;; Configuration ------------------------------------------------------------

(defparameter *version* "0.0.1")

(defparameter *separator* (string #\Newline))

(defparameter *interactive-input* *query-io*)

(defparameter *interactive-output* *query-io*)



;;;; Functionality ------------------------------------------------------------

(defun read-lines (stream)

  (loop :for line = (read-line stream nil)

        :while line

        :collect line))


(defun matchesp (string choices)

  (member string choices :test #'string-equal))


(defun prompt (format-string &rest args)

  (loop :for line = (progn

                      (apply #'format *interactive-output* format-string args)

                      (force-output *interactive-output*)

                      (read-line *interactive-input*))

        :do (cond

              ((matchesp line '("y" "yes")) (return t))

              ((matchesp line '("n" "no" "")) (return nil)))))


(defun filter-many (choices)

  (loop

    :with width = (1+ (reduce #'max choices :key #'length :initial-value 0))

    :for choice :in choices

    :when (prompt "~A~vA[yN] " choice (- width (length choice)) #\space)

    :collect choice))


(defun filter-one (choices)

  (loop :for choice :in choices

        :for i :from 0

        :do (format *interactive-output* "~36R) ~A~%" i choice)

        :collect choice)

  (let ((i (parse-integer (read-line *interactive-input*) :radix 36)))

    (if (or (minusp i) (>= i (length choices)))

      (error "Bad choice ~d" i)

      (list (elt choices i)))))


(defun output (results)

  (loop :for (r . remaining) :on results

        :do (write-string r)

        :when remaining :do (write-string *separator*)))


(defun run-many (choices)

  (output (filter-many choices)))


(defun run-one (choices)

  (output (filter-one choices)))



;;;; User Interface -----------------------------------------------------------

(defparameter *examples*

  '(("Pick some Python files and count their lines:"

     . "wc -l `pick *.py`")

    ("Search for some processes and interactively pick some to kill:"

     . "ps aww | awk 'NR > 1 { print $1, $5 }' | grep htop | pick | cut -d' ' -f1 | xargs kill")))



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


(defparameter *option-separator*

  (adopt:make-option 'separator

    :help "Print SEP between records when outputting (default: newline)."

    :long "separator"

    :short #\s

    :initial-value *separator*

    :parameter "SEP"

    :reduce #'adopt:last))


(defparameter *option-null*

  (adopt:make-option 'null

    :result-key 'separator

    :help "Use null bytes as separators for output."

    :long "null"

    :short #\0

    :reduce (constantly (string #\nul))))


(defparameter *option-one*

  (adopt:make-option 'one

    :help "Pick a single line instead of picking one-by-one."

    :long "one"

    :short #\o

    :reduce (constantly t)

    :initial-value nil))


(defparameter *option-many*

  (adopt:make-option 'many

    :result-key 'one

    :help "Pick multiple lines, asking one-by-one (the default)."

    :long "many"

    :short #\O

    :reduce (constantly nil)))



(adopt:define-string *help-text*

  "pick displays its arguments one-by-one on standard error and prompts you ~

   interactively to choose some of them.  The chosen items will be printed to ~

   standard output.~@

   ~@

   An argument of - will cause pick to read lines from standard input as ~

   choices.  Using an explicit - instead of reading from standard input when no ~

   arguments are present prevents something like 'pick `ls -1 | grep foo`' from ~

   silently hanging forever if no files match.~@

   ~@

   Using the --one argument changes the behaviour of pick.  Instead of ~

   picking several lines from the input by asking one-by-one, all of the lines ~

   are presented at once and the user is prompted to pick one of them with ~

   a prefix.

   ~@

   This version was inspired by the pick program described in 'The UNIX ~

   Programming Environment'.")


(defparameter *ui*

  (adopt:make-interface

    :name "pick"

    :usage "[OPTIONS]"

    :summary "interactively pick some things"

    :help *help-text*

    :examples *examples*

    :contents (list *option-help*

                    *option-version*

                    *option-separator*

                    *option-null*

                    *option-one*

                    *option-many*)))



(defmacro exit-on-ctrl-c (&body body)

  `(handler-case

       (with-user-abort:with-user-abort (progn ,@body))

     (with-user-abort:user-abort () (adopt:exit 130))))


(defun toplevel ()

  (exit-on-ctrl-c

    (handler-case

        (multiple-value-bind (arguments options) (adopt:parse-options *ui*)

          (cond ((gethash 'help options) (adopt:print-help-and-exit *ui* :option-width 24))

                ((gethash 'version options) (write-line *version*) (adopt:exit)))

          (with-open-file (*interactive-input* "/dev/tty" :direction :input)

            (let ((*separator* (gethash 'separator options))

                  (*interactive-output* *error-output*)

                  (arguments (mapcan (lambda (arg)

                                       (if (string= "-" arg)

                                         (read-lines *standard-input*)

                                         (list arg)))

                                     arguments)))

              (funcall (if (gethash 'one options)

                         #'run-one

                         #'run-many)

                       arguments))))

      (error (c) (adopt:print-error-and-exit c)))))

