(defpackage :ps-experiment-tests-for-utils/failure
  (:use :cl
        :prove
        :parenscript
        :ps-experiment
        :ps-experiment/t/test-utils))
(in-package :ps-experiment-tests-for-utils/failure)

;; Test the case only JavaScript is failed.

(defun.ps+ success-func () 0)

(defun failure-func () 1)
(defun.ps-only failure-func () -1)

(deftest.ps+ failure-case
  (ok (= (success-func) 0))
  ;; fail only ps side
  (ok (= (failure-func) 1)))
