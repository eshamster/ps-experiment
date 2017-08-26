(in-package :cl-user)
(defpackage ps-experiment-test.defines
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment-test.test-utils
        :prove)
  (:import-from :ps-experiment.package
                :unintern-all-ps-symbol))
(in-package :ps-experiment-test.defines)

(plan 6)

(defvar.ps a 20)

(subtest
    "Test defvar.ps"
  (is (execute-js
       (with-use-ps-pack (:this)
         (defun test (x)
           (incf a x))
         (test 100)
         a))
      120))

(defmacro exec-in-this (&body body)
  `(execute-js (with-use-ps-pack (:this)
                 ,@body)))

(defun.ps test-fn0 () 0)
(defun.ps test-fn1 (a b) (+ a b))
(defun.ps-only test-fn2 (a b) (+ a b))

(subtest
    "Test defun.ps"
  (subtest
      "Test functions defined by defun.ps"
    (is (exec-in-this (test-fn0)) 0)
    (is (exec-in-this
          (test-fn1 10 20))
        30)
    ;; The CL function is defined but not implemented
    (is-error (test-fn1 10 20)
              'simple-error))
  (subtest
      "Test functions defined by defun.ps-only"
    (is (exec-in-this
          (test-fn2 10 20))
        30)
    (is-error (test-fn2 10 20)
              'undefined-function)))

(defstruct.ps test-str1 a1 (b1 20))
(defvar.ps s (new (test-str1))
  "Test comment")
(defstruct.ps test-str2 (a2 s.b1))

(defstruct.ps parent (a 10) (b 20))
(defstruct.ps (child (:include parent)) (c (lambda () 10)))
(defstruct.ps (mod-child (:include parent (a 100))))

(subtest
    "Test defstruct.ps"
  (subtest
      "Test return value"
    (is (defstruct.ps test-defstruct-return-value a b c)
        'test-defstruct-return-value)
    (is (defstruct.ps (test-defstruct-return-value-child (:include parent)))
        'test-defstruct-return-value-child))
  (subtest
      "Test initilization"
    (is (exec-in-this (defvar x (new (test-str2))) 
                      x.a2)
        20))
  (subtest
      "Test make-... function"
    (ok (exec-in-this (defvar x (make-test-str1 :b1 200))
                      (test-str1-p x)))
    (is (exec-in-this (defvar x
                        (make-test-str1 :b1 200 :a1 100))
                      x.b1)
        200))
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
      "Test if CL function is defined but not implemented"
    (is-error (test-fn1 10 20)
              'simple-error))
  (subtest
      "Test accessors"
    (is (exec-in-this
         (test-str1-a1 (make-test-str1 :a1 100)))
        100)
    (is (exec-in-this
         (defvar x (make-test-str1 :a1 100))
         (setf (test-str1-a1 x) 200)
         x.a1)
        200))
  (subtest
      "Test inheritance"
    (subtest
        "Test type check"
      (ok (exec-in-this
           (parent-p (new (child)))))
      (ok (exec-in-this
           (child-p (new (child)))))
      (ok (not (exec-in-this
                (child-p (new (parent)))))))
    (subtest
        "Test make and accessors"
      (is (exec-in-this
           (defvar x (make-child :a 100))
           (parent-a x))
          100)
      (is (exec-in-this
           (defvar x (make-child :a 100))
           (child-a x))
          100)
      (is (exec-in-this
           (defvar x (make-child :a 100 :c 200))
           (+ x.a x.c))
          300))
    (subtest
        "Test override the initial value of parent's slot"
      (is (exec-in-this
           (mod-child-a (make-mod-child)))
          100)))
  (subtest
      "Test syntax errors"
    (subtest
        "Test struct name"
      (prove-macro-expand-error (defstruct.ps 12 a b) 'type-error
                                :expand-times 2)
      (prove-macro-expand-error (defstruct.ps "test" a b) 'type-error
                                :expand-times 2))
    (subtest
        "Test option"
      (prove-macro-expand-error (defstruct.ps (test (:include 123) a b)) 'type-error
                                :expand-times 2)
      (prove-macro-expand-error (defstruct.ps (test (:include "test") a b)) 'type-error
                                :expand-times 2)
      (prove-macro-expand-error (defstruct.ps (test (:not-exist abc) a b)) 'simple-error
                                :expand-times 2)))
    (subtest
        "Test slot name"
      (prove-macro-expand-error (defstruct.ps test_error 12 b) 'type-error
                                :expand-times 2)
      (prove-macro-expand-error (defstruct.ps test "test" b) 'type-error
                                :expand-times 2)
      (prove-macro-expand-error (defstruct.ps test a (12 12)) 'type-error
                                :expand-times 2)
      (prove-macro-expand-error (defstruct.ps test_duplicated-name a b a) 'simple-error
                                :expand-times 2))
    (subtest
        "Test override the initial value of parent's slot"
      (prove-macro-expand-error (defstruct.ps (test (:include parent (not-found 20)))) 'simple-error
                                :expand-times 2)
      (prove-macro-expand-error (defstruct.ps (test (:include parent a))) 'simple-error
                                :expand-times 2)
      (prove-macro-expand-error (defstruct.ps (test (:include parent (a 20 30)))) 'simple-error
                                :expand-times 2))
    (subtest
        "Test not-exist included-structure-name"
      (prove-macro-expand-error (defstruct.ps (test (:include not-exist))) 'unbound-variable
                                :expand-times 2)))

(defun.ps+ test-func-plus (a b)
  (+ a b))
(defvar.ps+ test-var-plus 100)

(defstruct.ps+ test-struct-plus1 (a 100) b)
(defstruct.ps+ (test-struct-plus2 (:include test-struct-plus1)) c)
(defvar.ps+ str-plus (make-test-struct-plus2 :c 20)
  "Test comment")

(subtest
    "Test xxx.ps+ macros"
  (with-prove-in-both ()
    (is test-var-plus 100)
    (is (test-struct-plus2-a str-plus) 100)
    (is (test-struct-plus2-c str-plus) 20)
    (ok (test-struct-plus1-p str-plus))
    (ok (not (let ((target (make-test-struct-plus1)))
               (test-struct-plus2-p target))))))

(unintern-all-ps-symbol)
(is (hash-table-count ps-experiment.defines::*ps-struct-slots*) 0)

;; ----- Test internals ----- ;;

(subtest
    "Test internal functions"
  (labels ((fn (lambda-list)
             (ps-experiment.defines::extract-arg-names lambda-list))
           (is-list (got expected)
             (is got expected :test #'equalp)))
    (is-list (fn '()) '())
    (is-list (fn '(a b )) '(a b))
    (is-list (fn '(a b &optional (c 0) d)) '(a b c d))
    (is-list (fn '(a b &optional (c 0) d &key (e 0) f)) '(a b c d e f))
    (is-list (fn '(a b &optional (c 0) d &rest rest)) '(a b c d rest))
    (is-list (fn '(a b &aux (au 0) x)) '(a b au x))))

(finalize)
