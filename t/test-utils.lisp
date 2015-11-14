(in-package :cl-user)
(defpackage ps-experiment-test.test-utils
  (:use :cl)
  (:import-from :cl-js
                :run-js
                :with-js-env
                :empty-lib
                :undefined-variable)
  (:export :execute-js
           :undefined-variable))
(in-package :ps-experiment-test.test-utils)

(defun execute-js (js-str)
  (with-js-env ((empty-lib))
    (run-js js-str)))
