(in-package :cl-user)
(defpackage ps-experiment-test.test-utils
  (:use :cl
        :prove)
  (:import-from :ps-experiment
                :with-use-ps-pack
                :ps.)
  (:import-from :cl-js
                :run-js
                :js-condition
                :with-js-env
                :empty-lib
                :undefined-variable)
  (:import-from :parenscript
                :ps)
  (:import-from :alexandria
                :with-gensyms)
  (:export :execute-js
           :prove-macro-expand-error
           :prove-psmacro-expand-error
           :prove-in-both
           :is-list.ps+
           :undefined-variable))
(in-package :ps-experiment-test.test-utils)

(defun execute-js (js-str)
  (with-js-env ((empty-lib))
    (run-js js-str)))

(defmacro prove-macro-expand-error (code expected-error)
  `(is-error (macroexpand-1 ',code)
             ,expected-error))

(defmacro prove-psmacro-expand-error (code expected-error)
  `(is-error (macroexpand-1 '(ps ,code))
             ,expected-error))

(defmacro prove-in-both ((prove body &rest rest) &key (use '(:this)) (prints-js nil))
  (with-gensyms (js)
    `(progn
       (princ "Common Lisp: ")
       (fresh-line)
       (,prove ,body ,@rest)
       (princ "JavaScript: ")
       (fresh-line)
       (let ((,js (with-use-ps-pack ,use ,body)))
         (when ,prints-js
           (print ,js))
         ,(if (eq prove 'prove:is-error)
              `(,prove (run-js ,js) 'js-condition)
              `(,prove (run-js ,js) ,@rest)))
       (princ "------")
       (fresh-line))))

(defun js-array-to-list (js-array)
  (let ((result nil))
    (dotimes (i (cl-js:js-array-length js-array))
      (push (cl-js:js-aref js-array i) result))
    (nreverse result)))

(defmacro is-list.ps+ (got expected)
  (if (not (listp expected))
      (error 'type-error :expected-type 'list :datum expected))
  (with-gensyms (js-got js-expected)
    `(progn
       (print "Common Lisp: ")
       (is ,got ,expected :test #'equalp)
       (print "JavaScript: ")
       (let ((,js-got (cl-js:run-js (ps. ,got)))
             (,js-expected (cl-js:run-js (ps. ,expected))))
         (is ,js-got ,js-expected :test #'equalp
             (format nil "~A is expected (got: ~A)"
                     (js-array-to-list ,js-expected)
                     (js-array-to-list ,js-got)))))))
