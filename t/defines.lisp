(in-package :cl-user)
(defpackage ps-experiment-test.defines
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment-test.test-utils
        :prove)
  (:import-from :ps-experiment.package
                :with-use-ps-pack
                :find-ps-symbol
                :unintern-all-ps-symbol))
(in-package :ps-experiment-test.defines)

(plan 2)

(defvar.ps a 20)

(subtest
    "Test defvar.ps"
  (is (execute-js
       (with-use-ps-pack (:this)
         (defun test (x)
           (incf a x))
         (test 100)
         a))
      120)
  (ok (find-ps-symbol "_DEFVAR_A")))


(defstruct.ps test-str1 a1 (b1 20))
(defvar.ps s (new (test-str1)))
(defstruct.ps test-str2 (a2 s.b1))

(subtest
    "Test defstruct.ps"
  (is (execute-js
       (with-use-ps-pack (:this)
         (defvar x (new (test-str2))) 
         x.a2))
      20)
  (ok (find-ps-symbol "_DEFSTRUCT_TEST-STR1")))

(finalize)

(unintern-all-ps-symbol)
