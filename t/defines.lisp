(in-package :cl-user)
(defpackage ps-experiment/t/defines
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment/t/test-utils
        :rove)
  (:import-from :ps-experiment/package
                :unintern-all-ps-symbol))
(in-package :ps-experiment/t/defines)

(defvar.ps a 20)

(deftest for-defvar.ps
  (ok (= (execute-js
          (with-use-ps-pack (:this)
            (defun test (x)
              (incf a x))
            (test 100)
            a))
         120)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro exec-in-this (&body body)
    `(execute-js (with-use-ps-pack (:this)
                   ,@body))))

(defun.ps test-fn0 () 0)
(defun.ps test-fn1 (a b) (+ a b))
(defun.ps-only test-fn2 (a b) (+ a b))

(deftest for-defun.ps
  (testing "Test functions defined by defun.ps"
    (ok (= (exec-in-this (test-fn0)) 0))
    (ok (= (exec-in-this
             (test-fn1 10 20))
           30))
    ;; The CL function is defined but not implemented
    (ok (signals (test-fn1 10 20)
                 'simple-error)))
  (testing "Test functions defined by defun.ps-only"
    (ok (= (exec-in-this
             (test-fn2 10 20))
           30))
    (ok (signals (test-fn2 10 20)
                 'undefined-function))))



(defstruct.ps test-str1 a1 (b1 20))
(defvar.ps s (new (test-str1))
  "Test comment")
(defstruct.ps test-str2 (a2 s.b1))

(defstruct.ps parent (a 10) (b 20))
(defstruct.ps (child (:include parent)) (c (lambda () 10)))
(defstruct.ps (mod-child (:include parent (a 100))))

(deftest for-defstruct.ps
  (testing "Test return value"
    (ok (eq (defstruct.ps test-defstruct-return-value a b c)
            'test-defstruct-return-value))
    (ok (eq (defstruct.ps (test-defstruct-return-value-child (:include parent)))
            'test-defstruct-return-value-child)))
  (testing "Test initilization"
    (ok (= (exec-in-this (defvar x (new (test-str2)))
                         x.a2)
           20)))
  (testing "Test make-... function"
    (ok (exec-in-this (defvar x (make-test-str1 :b1 200))
                      (test-str1-p x)))
    (ok (= (exec-in-this (defvar x
                           (make-test-str1 :b1 200 :a1 100))
                         x.b1)
           200)))
  (testing "Test ...-p function"
    (ok (exec-in-this
         (test-str1-p (new (test-str1)))))
    (ng (exec-in-this
          (test-str1-p (new (test-str2)))))
    (ng (exec-in-this
          (test-str1-p 1)))
    (ng (exec-in-this
          (test-str1-p "test"))))
  (testing "Test if CL function is defined but not implemented"
    (ok (signals (test-fn1 10 20)
                 'simple-error)))
  (testing "Test accessors"
    (ok (= (exec-in-this
             (test-str1-a1 (make-test-str1 :a1 100)))
           100))
    (ok (= (exec-in-this
             (defvar x (make-test-str1 :a1 100))
             (setf (test-str1-a1 x) 200)
             x.a1)
           200)))
  (testing "Test inheritance"
    (testing "Test type check"
      (ok (exec-in-this
            (parent-p (new (child)))))
      (ok (exec-in-this
            (child-p (new (child)))))
      (ng (exec-in-this
            (child-p (new (parent))))))
    (testing "Test make and accessors"
      (ok (= (exec-in-this
               (defvar x (make-child :a 100))
               (parent-a x))
             100))
      (ok (= (exec-in-this
               (defvar x (make-child :a 100))
               (child-a x))
             100))
      (ok (= (exec-in-this
               (defvar x (make-child :a 100 :c 200))
               (+ x.a x.c))
             300)))
    (testing "Test override the initial value of parent's slot"
      (ok (= (exec-in-this
               (mod-child-a (make-mod-child)))
             100))))
  (testing "Test syntax errors"
    (testing "Test struct name"
      (ok (signals-when-expand (defstruct.ps 12 a b) 'type-error
                               :expand-times 2))
      (ok (signals-when-expand (defstruct.ps "test" a b) 'type-error
                               :expand-times 2)))
    (testing
        "Test option"
      (ok (signals-when-expand (defstruct.ps (test (:include 123) a b)) 'type-error
                               :expand-times 2))
      (ok (signals-when-expand (defstruct.ps (test (:include "test") a b)) 'type-error
                               :expand-times 2))
      (ok (signals-when-expand (defstruct.ps (test (:not-exist abc) a b)) 'simple-error
                               :expand-times 2)))
    (testing "Test slot name"
      (ok (signals-when-expand (defstruct.ps test_error 12 b) 'type-error
                               :expand-times 2))
      (ok (signals-when-expand (defstruct.ps test "test" b) 'type-error
                               :expand-times 2))
      (ok (signals-when-expand (defstruct.ps test a (12 12)) 'type-error
                               :expand-times 2))
      (ok (signals-when-expand (defstruct.ps test_duplicated-name a b a) 'simple-error
                               :expand-times 2)))
    (testing
        "Test override the initial value of parent's slot"
      (ok (signals-when-expand (defstruct.ps (test (:include parent (not-found 20)))) 'simple-error
                               :expand-times 2))
      (ok (signals-when-expand (defstruct.ps (test (:include parent a))) 'simple-error
                               :expand-times 2))
      (ok (signals-when-expand (defstruct.ps (test (:include parent (a 20 30)))) 'simple-error
                               :expand-times 2)))
    (testing "Test not-exist included-structure-name"
      (ok (signals-when-expand (defstruct.ps (test (:include not-exist))) 'unbound-variable
                               :expand-times 2)))))

(defun.ps+ test-func-plus (a b)
  (+ a b))
(defvar.ps+ test-var-plus 100)

(defstruct.ps+ test-struct-plus1 (a 100) b)
(defstruct.ps+ (test-struct-plus2 (:include test-struct-plus1)) c)
(defvar.ps+ str-plus (make-test-struct-plus2 :c 20)
  "Test comment")

(deftest.ps+ for-xxx.ps+-macros
  (ok (= test-var-plus 100))
  (ok (= (test-struct-plus2-a str-plus) 100))
  (ok (= (test-struct-plus2-c str-plus) 20))
  (ok (test-struct-plus1-p str-plus))
  (ng (let ((target (make-test-struct-plus1)))
        (test-struct-plus2-p target))))

(defvar.ps+ *test-hoge* 100)
(defun.ps+ test-get-hoge () *test-hoge*)
(defsetf.ps+ test-get-hoge
    () (value)
    `(setf *test-hoge* ,value))

(deftest.ps+ for-defsetf
  (let ((pre-value *test-hoge*))
    (unwind-protect
         (progn (ok (= (test-get-hoge) 100))
                (setf (test-get-hoge) 200)
                (ok (= (test-get-hoge) 200)))
      (setf *test-hoge* pre-value))))

;; ----- Test internals ----- ;;

(deftest internal-functions
  (labels ((fn (lambda-list)
             (ps-experiment/defines::extract-arg-names lambda-list))
           (is-list (got expected)
             (ok (equalp got expected))))
    (is-list (fn '()) '())
    (is-list (fn '(a b )) '(a b))
    (is-list (fn '(a b &optional (c 0) d)) '(a b c d))
    (is-list (fn '(a b &optional (c 0) d &key (e 0) f)) '(a b c d e f))
    (is-list (fn '(a b &optional (c 0) d &rest rest)) '(a b c d rest))
    (is-list (fn '(a b &aux (au 0) x)) '(a b au x))))
