(in-package :cl-user)
(defpackage ps-experiment-test.test-utils
  (:use :cl
        :prove)
  (:import-from :cl-js
                :run-js
                :with-js-env
                :empty-lib
                :undefined-variable)
  (:import-from :parenscript
                :ps)
  (:export :execute-js
           :prove-macro-expand-error
           :prove-psmacro-expand-error
           :undefined-variable))
(in-package :ps-experiment-test.test-utils)

(defun execute-js (js-str)
  (with-js-env ((empty-lib))
    (run-js js-str)))

(defmacro prove-macro-expand-error (code expected-error)
  `(is-error (eval (read-from-string ,(format nil "~S" code)))
             ,expected-error))

(defmacro prove-psmacro-expand-error (code expected-error)
  `(is-error (eval (read-from-string ,(format nil "~S" `(ps ,code))))
             ,expected-error))
