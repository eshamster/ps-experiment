(in-package :cl-user)
(defpackage ps-experiment-test.test-utils
  (:use :cl
        :prove)
  (:import-from :cl-js
                :run-js
                :with-js-env
                :empty-lib
                :undefined-variable)
  (:export :execute-js
           :prove-macro-expand-error
           :undefined-variable))
(in-package :ps-experiment-test.test-utils)

(defun execute-js (js-str)
  (with-js-env ((empty-lib))
    (run-js js-str)))

(defmacro prove-macro-expand-error (code expected-error)
  `(is-error (eval (read-from-string ,(format nil "~S" code)))
             ,expected-error))
