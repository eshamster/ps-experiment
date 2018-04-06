(defpackage ps-experiment-tests-for-utils/success
  (:use :cl
        :rove
        :parenscript
        :ps-experiment
        :ps-experiment/t/test-utils))
(in-package :ps-experiment-tests-for-utils/success)

(defun.ps+ success-func1 () 0)
(defun.ps+ success-func2 () 1)

(deftest.ps+ success-case
  (ok (= (success-func1) 0))
  (ok (= (success-func2) 1)))
