;; Fork of lispindent https://github.com/ds26gte/scmindent

;; Some day I need to rewrite this with smug or something, jesus.


(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload '(:adopt) :silent t))


(defpackage :lispindent

  (:use :cl)

  (:export :toplevel :*ui*))


(in-package :lispindent)



;Dorai Sitaram

;Oct 8, 1999

;last change 2014-09-16


;this script takes lines of Lisp or Scheme code from its

;stdin and produces an indented version thereof on its

;stdout


(defun safe-read (stream)

  (let ((*read-eval* nil))

    (read stream nil)))


(defun safe-read-from-string (string)

  (let ((*read-eval* nil))

    (read-from-string string)))



(defvar *lisp-keywords* '())

(defvar *labels-keywords* '(labels flet macrolet labels* flet* labels-memoized))

(defvar *labels2-keywords* '(state-machine))

(defvar *lambda-list-keywords* '(defun defmethod defmacro define-compiler-macro defun* defmacro-clause defmacro-driver))

(defvar *case-keywords* '(cond case))



(defun define-with-lisp-indent-number (n syms)

  (dolist (sym syms)

    (let* ((x (symbol-name sym))

           (c (assoc x *lisp-keywords* :test #'string-equal)))

      (unless c

        (push (setq c (cons x nil)) *lisp-keywords*))

      (setf (cdr c) n))))


(define-with-lisp-indent-number 0

  '(block

    handler-bind

    loop))


(define-with-lisp-indent-number 1

  '(case

    defpackage do-all-symbols do-external-symbols dolist do-symbols dotimes

    ecase etypecase eval-when

    flet

    handler-case

    labels lambda let let* let-values

    macrolet

    prog1

    typecase

    unless unwind-protect

    when with-input-from-string with-open-file with-open-socket

    with-open-stream with-output-to-string))


(define-with-lisp-indent-number 2

  '(assert

    defun destructuring-bind do do*

    if

    multiple-value-bind

    with-slots))



(defparameter *config-files*

  (list (merge-pathnames ".lispwords" (user-homedir-pathname))

        ".lispwords"

        ".notmylispwords"))



(defun source-config-files ()

  (dolist (path *config-files*)

    (with-open-file (i path :if-does-not-exist nil)

      (when i

        (loop

          (let ((w (or (safe-read i) (return))))

            (define-with-lisp-indent-number (car w) (cdr w))))))))



(defstruct lparen

  word

  spaces-before

  num-aligned-subforms

  (num-finished-subforms 0)

  (num-processed-subforms 0))



(defun past-next-token (s i n)

  (let ((escapep nil))

    (loop

      (when (>= i n) (return i))

      (let ((c (char s i)))

        (cond (escapep (setq escapep nil))

              ((char= c #\\) (setq escapep t))

              ((char= c #\#)

               (let ((j (+ i 1)))

                 (if (>= j n) (return i)

                   (let ((c (char s j)))

                     (cond ((char= c #\\) (setq escapep t i j))

                           (t (return i)))))))

              ((member c '(#\space #\tab #\{ #\} #\( #\) #\[ #\] #\" #\' #\` #\, #\;))

               (return i))))

      (incf i))))


(defun lisp-indent-number (s &optional (possible-keyword-p t))

  (or (cdr (assoc s *lisp-keywords* :test #'string-equal))

      (if (zerop (or (search "def" s :test #'char-equal)

                     (search "with-" s :test #'char-equal)

                     -1))

        0

        (if possible-keyword-p

          (let ((p (position #\: s :from-end t)))

            (if p

              (lisp-indent-number (subseq s (1+ p)) nil)

              -1))

          -1))))


(defun literal-token-p (s)

  (let ((colon-pos (position #\: s)))

    (if colon-pos

        (if (= colon-pos 0) t nil)

      (let ((s (safe-read-from-string s)))

        (or (eql t s) (characterp s) (numberp s) (stringp s))))))


;(trace lisp-indent-number literal-token-p read-from-string past-next-token)


(defun current-word (s i n)

  (let ((j (past-next-token s i n)))

    (when (not (= i j))

      (subseq s i j))))



(defun in-labels-p (stack)

  (let ((target (cadr stack)))

    (or (and target

             (member (lparen-word target) *labels-keywords*

                     :key #'symbol-name :test #'string-equal)

             (= (lparen-num-processed-subforms target) 0))

        (and target

             (member (lparen-word target) *labels2-keywords*

                     :key #'symbol-name :test #'string-equal)

             (= (lparen-num-processed-subforms target) 1)))))


(defun in-lambda-list-p (stack)

  (let ((target (car stack)))

    (and target

         (member (lparen-word target) *lambda-list-keywords*

                 :key #'symbol-name :test #'string-equal)

         (= (lparen-num-processed-subforms target) 0))))


(defun in-case-p (stack)

  (let ((target (car stack)))

    (and target

         (member (lparen-word target) *case-keywords*

                 :key #'symbol-name :test #'string-equal))))




(defun calc-subindent (stack s i n)

  (let* ((j (past-next-token s i n))

         (num-aligned-subforms 0)

         (left-indent

           (if (= j i) 1

             (let ((w (subseq s i j)))

               (cond

                 ((and (>= i 2) (member (char s (- i 2)) '(#\' #\`))) 2)

                 ((and (>= i 2) (member (subseq s (- i 2) i) '("#(" "#{" "#[") :test 'string=) 1))

                 ((and (>= i 1) (member (subseq s (- i 1) i) '("[" "{") :test 'string=) 1))

                 (t (let ((nas (if (in-labels-p stack)

                                 1

                                 (lisp-indent-number w))))

                      (cond ((or (in-lambda-list-p stack)

                                 (in-case-p stack)) 1)

                            ((>= nas 0) (setq num-aligned-subforms nas)

                             2)

                            ((literal-token-p w) 1)

                            ((= j n) 2)

                            (t (+ (- j i) 2))))))))))

    (values left-indent num-aligned-subforms (1- j))))


;; (trace calc-subindent)


(defun num-leading-spaces (s)

  (let ((n (length s))

        (i 0) (j 0))

    (loop

      (when (>= i n) (return 0))

      (case (char s i)

        (#\space (incf i) (incf j))

        (#\tab (incf i) (incf j 8))

        (t (return j))))))



(defun string-trim-blanks (s)

  (string-trim '(#\space #\tab #\newline #\return) s))



(defun indent-lines ()

  (let ((left-i 0)

        (paren-stack '())

        (stringp nil))

    (loop

      (let* ((curr-line (or (read-line nil nil) (return)))

             (leading-spaces (num-leading-spaces curr-line))

             (curr-left-i

               (cond (stringp leading-spaces)

                     ((null paren-stack)

                      (when (= left-i 0) (setq left-i leading-spaces))

                      left-i)

                     (t (let* ((lp (car paren-stack))

                               (nas (lparen-num-aligned-subforms lp))

                               (nfs (lparen-num-finished-subforms lp))

                               (extra-w 0))

                          (when (< nfs nas) ;(and (>= nas 0) (< nfs nas))

                            (incf (lparen-num-finished-subforms lp))

                            (setq extra-w 2))

                          (+ (lparen-spaces-before lp)

                             extra-w))))))

        (setq curr-line (string-trim-blanks curr-line))

        (when (not (string= curr-line "")) ; don't add "trailing" whitespace

          (dotimes (k curr-left-i) (write-char #\space)))

        (princ curr-line)

        (terpri)

        ;

        (let ((i 0) (n (length curr-line)) (escapep nil)

              (inter-word-space-p nil))

          (loop

            (when (>= i n) (return))

            (let ((c (char curr-line i)))

              (cond (escapep (setq escapep nil))

                    ((char= c #\\) (setq escapep t))

                    (stringp (when (char= c #\") (setq stringp nil)))

                    ((char= c #\;) (return))

                    ((char= c #\") (setq stringp t))

                    ((member c '(#\space #\tab) :test #'char=)

                     (unless inter-word-space-p

                       (setq inter-word-space-p t)

                       (let ((lp (car paren-stack)))

                         (when lp

                           (incf (lparen-num-finished-subforms lp))))))

                    ((member c '(#\( #\[ #\{) :test #'char=)

                     (setq inter-word-space-p nil)

                     (multiple-value-bind (left-indent num-aligned-subforms j)

                         (calc-subindent paren-stack curr-line (1+ i) n)

                       (push

                         (make-lparen :word (current-word curr-line (1+ i) n)

                                      :spaces-before (+ i curr-left-i left-indent)

                                      :num-aligned-subforms num-aligned-subforms)

                         paren-stack)

                       (setq i j)))

                    ((member c '(#\) #\] #\}) :test #'char=)

                     (setq inter-word-space-p nil)

                     (cond (paren-stack (pop paren-stack))

                           (t (setq left-i 0)))

                     (let ((lp (car paren-stack)))

                       (when lp

                         (incf (lparen-num-processed-subforms lp)))))

                    (t (setq inter-word-space-p nil)))

              (incf i))))))))



(defun run ()

  (source-config-files)

  (indent-lines)

  t)



(adopt:define-string *doc-help*

  "Read Common Lisp code from standard input, indent it according to some ~

   simple rules and configuration, and write the result to standard output.")


(adopt:define-string *doc-manual*

  "~A~@

   ~@

   Configuration is read from ~~/.lispwords and ./.lispwords"

   *doc-help*)


(defparameter *ui*

  (adopt:make-interface

    :name "lispindent"

    :usage "[OPTIONS]"

    :summary "Indent Common Lisp code."

    :help *doc-help*

    :manual *doc-manual*

    :contents

    (list (adopt:make-option 'help

            :help "display help and exit"

            :manual "Display help and exit."

            :long "help"

            :short #\h

            :reduce (constantly t)))))



(defun toplevel ()

  (handler-case

      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)

        (when (gethash 'help options)

          (adopt:print-help-and-exit *ui*))

        (when arguments

          (error "Unrecognized arguments: ~S" arguments))

        (run))

    (error (c) (adopt:print-error-and-exit c))))
