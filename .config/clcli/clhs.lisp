;; Based on https://gist.github.com/fukamachi/3510ea1609c1b52830c2


(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload '(:adopt :drakma :plump :clss :alexandria)

                :silent t))


(defpackage :clhs

  (:use :cl)

  (:export :toplevel :*ui*))


(in-package :clhs)



;;;; Config -------------------------------------------------------------------

(defparameter *default-hyperspec-url* "http://www.lispworks.com/documentation/HyperSpec/")

;; (defparameter *default-hyperspec-url* "file:///home/sjl/Dropbox/HyperSpec/HyperSpec/")

(defparameter *default-open-command* "open")

(defparameter *open* *default-open-command*)

(defparameter *url* *default-hyperspec-url*)

(defparameter *quiet* nil)



;;;; Curl ---------------------------------------------------------------------

(defun retrieve-url (url)

  (tagbody

    retry

    (multiple-value-bind (body status) (drakma:http-request url)

      (unless (= status 200)

        (restart-case

            (error "Failed to retrieve ~S (Code=~A)" url status)

          (retry-request ()

                         :report "Retry the request to URL."

                         (go retry))))

      (return-from retrieve-url body))))


(defun retrieve-file (path)

  (alexandria:read-file-into-string path))



(defun url (target)

  (format nil "~A~A" *url* target))


(defun retrieve (target)

  (let ((path-or-url (url target)))

    (if (string= "file://" path-or-url :end2 7)

      (retrieve-file (subseq path-or-url 7))

      (retrieve-url path-or-url))))



;;;; Cache --------------------------------------------------------------------

(defun cache-directory ()

  (let ((cache-dir

          (uiop:ensure-directory-pathname

            (uiop:getenv "XDG_CACHE_HOME")

            (merge-pathnames ".cache/" (user-homedir-pathname)))))

    (merge-pathnames #P"clhs/" cache-dir)))


(defun cache-file ()

  (merge-pathnames #P"symbols-map.sexp" (cache-directory)))


(defun retrieve-symbol-map ()

  (let ((body (retrieve "Front/X_AllSym.htm")))

    (map 'list

         (lambda (a)

           (cons (plump:text a)

                 (let ((path (plump:attribute a "href")))

                   ;; Omit "../" and URL fragment

                   (subseq path 3 (position #\# path)))))

         (clss:select "a[rel=definition]" (plump:parse body)))))


(defun rebuild-cache ()

  (let ((cache (cache-file)))

    (format t "Rebuilding cache at ~A~%" cache)

    (let ((symbols (retrieve-symbol-map)))

      (ensure-directories-exist cache)

      (with-open-file (out cache

                           :direction :output

                           :if-exists :supersede

                           :if-does-not-exist :create)

        (prin1 symbols out))

      symbols)))


(defun symbol-map ()

  (let ((cache (cache-file)))

    (if (probe-file cache)

      (uiop:read-file-form cache)

      (rebuild-cache))))


(defun find-symbol-path (target-symbol)

  (cdr (assoc target-symbol (symbol-map) :test #'string-equal)))


(defun run (target-symbol)

  (let ((path (find-symbol-path target-symbol)))

    (if path

      (let ((url (url path)))

        (unless *quiet*

          (format t "Opening ~A~%" url))

        (uiop:run-program `(,*open* ,url)

                          :ignore-error-status t

                          :input :interactive

                          :output :interactive)

        t)

      nil)))



;;;; User Interface -----------------------------------------------------------

(adopt:define-string *documentation*

  "Look up SYMBOL in the Common Lisp HyperSpec and open its page in a web browser.~@

  ~@

  The first time the program is run it will create a cache of all symbols in ~

  the HyperSpec.  Subsequent runs will use this cache to look up symbol links ~

  faster.")


(defparameter *examples*

  '(("Look up STRING in the HyperSpec and open it in the default browser:" .

     "clhs string")

    ("Look up MAKE-ARRAY in a local copy of the HyperSpec and open it in lynx:" .

     "clhs --url file:///home/sjl/doc/hyperspec/ --open lynx make-array")))



(defparameter *o-help*

  (adopt:make-option 'help

    :help "display help and exit"

    :manual "Display help and exit."

    :long "help"

    :short #\h

    :reduce (constantly t)))


(defparameter *o-quiet*

  (adopt:make-option 'quiet

    :help "don't display extra logging output"

    :manual "Don't display any extra logging output."

    :long "quiet"

    :short #\q

    :reduce (constantly t)))


(defparameter *o-rebuild-cache*

  (adopt:make-option 'rebuild-cache

    :help "rebuild the symbol cache"

    :manual "Rebuild the symbol cache, even if it already exists, instead of looking up a symbol."

    :long "rebuild-cache"

    :reduce (constantly t)))


(defparameter *o-open*

  (adopt:make-option 'open

    :help (format nil "program to use to open hyperspec URLs (default ~A)" *default-open-command*)

    :manual (format nil "The program to use to open hyperspec URLs.  The default is: ~A" *default-open-command*)

    :long "open"

    :short #\o

    :parameter "COMMAND"

    :initial-value *default-open-command*

    :reduce #'adopt:last))


(defparameter *o-url*

  (adopt:make-option 'url

    :help (format nil "base HyperSpec URL (default ~A)" *default-hyperspec-url*)

    :manual (format nil

                    "The base HyperSpec URL.  The default is: ~A~@

                     ~@

                     A local copy of the HyperSpec can be used with a file:// prefix."

                    *default-hyperspec-url*)

    :long "url"

    :short #\u

    :parameter "URL"

    :initial-value *default-hyperspec-url*

    :reduce #'adopt:last))



(defparameter *ui*

  (adopt:make-interface

    :name "clhs"

    :usage "[OPTIONS] SYMBOL"

    :summary "Look up a symbol in the Common Lisp HyperSpec."

    :help *documentation*

    :examples *examples*

    :contents (list *o-help*

                    *o-quiet*

                    *o-rebuild-cache*

                    *o-open*

                    *o-url*)))


(defun toplevel ()

  (handler-case

      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)

        (when (gethash 'help options)

          (adopt:print-help-and-exit *ui*))

        (let ((*open* (gethash 'open options))

              (*url* (gethash 'url options))

              (*quiet* (gethash 'quiet options))

              (target (first arguments)))

          (if (gethash 'rebuild-cache options)

            (rebuild-cache)

            (progn

              (when (/= (length arguments) 1)

                (cerror "Type a symbol"

                        "Exactly one symbol to look up must be provided (got ~D: ~S)"

                        (length arguments)

                        arguments)

                (setf arguments (list (read-line))))

              (unless (run target)

                (adopt:print-error-and-exit

                  (format nil "Symbol not found: ~A~%" target)))))))

    (error (c) (adopt:print-error-and-exit c))))
