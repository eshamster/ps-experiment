(in-package :cl-user)
(defpackage ps-experiment-test.package
  (:use :cl
        :ps-experiment.package
        :parenscript
        :ps-experiment-test.test-utils
        :prove))

(defmacro def-test-package (name)
  `(defpackage ,name
     (:use :cl
           :ps-experiment.package
           :parenscript)))

(def-test-package test.package.pack-a)
(def-test-package test.package.pack-b)

(in-package :test.package.pack-a)

(defun test-a1 ()
  "test-a1")

(defun test-a2 ()
  "test-a2")

(register-ps-func 'test-a1)
(register-ps-func 'test-a2)


(in-package :test.package.pack-b)

(defun test-b1 ()
  "test-b1")

(defun test-b2 ()
  "test-b2")

(register-ps-func 'test-b1)
(register-ps-func 'test-b2)


(in-package :ps-experiment-test.package)

(defun test1 ()
  "test1")

(register-ps-func 'test1)

(plan 2)

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

(unintern-all-ps-symbol)

(finalize)
