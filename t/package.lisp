(in-package :cl-user)
(defpackage ps-experiment-test.package
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment-test.test-utils
        :prove)
  (:import-from :ps-experiment.package
                :register-ps-func
                :find-ps-symbol
                :def-ps-definer
                :unintern-all-ps-symbol)
  (:import-from :alexandria
                :symbolicate))

(defmacro def-test-package (name)
  `(defpackage ,name
     (:use :cl
           :ps-experiment.package
           :parenscript)))

(def-test-package test.package.pack-a)
(def-test-package test.package.pack-b)

;; --- package a --- ;;
(in-package :test.package.pack-a)

(defun test-a1 ()
  "test-a1")

(defun test-a2 ()
  "test-a2")

(register-ps-func 'test-a1)
(register-ps-func 'test-a2)


;; --- package b --- ;;
(in-package :test.package.pack-b)

(defun test-b1 ()
  "test-b1")

(defun test-b2 ()
  "test-b2")

(register-ps-func 'test-b1)
(register-ps-func 'test-b2)


;; --- body --- ;;
(in-package :ps-experiment-test.package)

(defun test1 ()
  "test1")

(register-ps-func 'test1)

(plan 6)

(subtest
    "Test find-ps-symbol"
  (labels ((prove-return (args expected)
             (multiple-value-bind (left right)
                 (apply #'find-ps-symbol args)
               (is (list left right) expected :test #'equal))))
    (prove-return '("TEST1" "NOT-DEFINED-PACKAGE") '(nil nil))))

(subtest
    "Test with-use-ps-pack"
  (is (with-use-ps-pack (:test.package.pack-a
                         :test.package.pack-b))
      "test-a1
test-a2
test-b1
test-b2
"
      :test #'equal)
  
  (pass (with-use-ps-pack (:all)))
  
  (is (with-use-ps-pack (:this
                         :test.package.pack-a)
        (setf test.abc 100))
      "test1
test-a1
test-a2
test.abc = 100;"
      :test #'equal))

;; --- affect global env --- ;;
(use-package :test.package.pack-b)

(subtest
    "Test dependencies by 'use'"
  (is (with-use-ps-pack (:this))
      "test-b1
test-b2
test1
")
  (is (with-use-ps-pack (:test.package.pack-b
                         :this))
      "test-b1
test-b2
test1
"))

(in-package :test.package.pack-b)
(use-package :test.package.pack-a)
(in-package :ps-experiment-test.package)

(subtest
    "Test cascaded dependencies by 'use'"
  (is (with-use-ps-pack (:this))
      "test-a1
test-a2
test-b1
test-b2
test1
"))

;; --- affect global env --- ;;
(unintern-all-ps-symbol)

(def-ps-definer defhoge.ps (name value) ()
  `(defvar ,name ,value))

(defhoge.ps x 100)

(subtest
    "Test make-ps-definer"
  (ok (find-ps-symbol "_DEFHOGE.PS_X"))
  (is (with-use-ps-pack (:this))
      "var x = 100;
"
      :test #'equal))

;; --- affect global env --- ;;
(unintern-all-ps-symbol)

(def-top-level-form.ps test-top-level 
  (+ 1 2)
  (* 3 4))

(subtest
    "Test def-top-level-form.ps"
  (is (with-use-ps-pack (:this))
      "1 + 2;
3 * 4;
"))

(unintern-all-ps-symbol)

(finalize)
