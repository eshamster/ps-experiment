(in-package :cl-user)
(defpackage ps-experiment-test.test-utils
  (:use :cl
        :prove)
  (:import-from :ps-experiment
                :with-use-ps-pack)
  (:import-from :cl-js
                :run-js
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
       (,prove ,body ,@rest)
       (princ "JavaScript: ")
       (let ((,js (with-use-ps-pack ,use ,body)))
         (when ,prints-js
           (print ,js))
         (,prove (run-js ,js) ,@rest))
       (princ "------"))))

