(in-package :cl-user)
(defpackage ps-experiment-test.defines
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment-test.test-utils
        :prove)
  (:import-from :ps-experiment.package
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

(defmacro exec-in-this (&body body)
  `(execute-js (with-use-ps-pack (:this)
                 ,@body)))

(subtest
    "Test defstruct.ps"
  (ok (find-ps-symbol "_DEFSTRUCT_TEST-STR1"))
  (subtest
      "Test initilization"
    (is (exec-in-this (defvar x (new (test-str2))) 
                      x.a2)
        20))
  (subtest
      "Test ...-p function"
    (ok (exec-in-this
         (test-str1-p (new (test-str1)))))
    (ok (not (exec-in-this
              (test-str1-p (new (test-str2))))))
    (ok (not (exec-in-this
              (test-str1-p 1))))
    (ok (not (exec-in-this
              (test-str1-p "test")))))
  (subtest
      "Test syntax errors"
    (prove-macro-expand-error "(defstruct.ps 12 a b)" 'type-error)
    (prove-macro-expand-error "(defstruct.ps \"test\" a b)" 'type-error)))


(finalize)

(unintern-all-ps-symbol)
