(in-package :cl-user)
(defpackage ps-experiment/t/defines/defmethod
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment/t/test-utils
        :rove))
(in-package :ps-experiment/t/defines/defmethod)

;; Note: Omit test of defgeneric.ps-only and defmethod.ps-only

;; --- prepare --- ;;

(defstruct.ps+ test-type-a x)
(defstruct.ps+ (test-type-b (:include test-type-a)))

(defvar.ps+ *type-a* (make-test-type-a))
(defvar.ps+ *type-b* (make-test-type-b))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro exec-in-this (&body body)
    `(execute-js (with-use-ps-pack (:this)
                   ,@body))))

;; --- test --- ;;

(defgeneric.ps method1 (a b) (:documentation "test"))
(defmethod.ps method1 ((a test-type-a) (b test-type-b))
  100)

(deftest for-defmethod.ps
  ;; Note: The main purpose of this test is to test
  ;; if the empty CL method is declared.
  ;; Main tests including multiple dispatch will be done in the "for-defmethod.ps+".
  (ok (= (exec-in-this (method1 *type-a* *type-b*)) 100))
  ;; The CL method is defined but not implemented
  (ok (signals (method1 *type-a* *type-b*)
               'simple-error)))

(defgeneric.ps+ method2 (a1 a2))
(defmethod.ps+ method2 ((a1 test-type-a) a2)
  (declare (ignore a1 a2))
  ;; Note: Dirty hack for bug of Parenscript.
  ;; ps:ps ignores a first string or a string just after "declare" in lambda.
  ;; So add a meaningless "nil" before that.
  nil "(type-a nil)")
(defmethod.ps+ method2 (a1 (a2 test-type-b))
  (declare (ignore a1 a2))
  nil "(nil type-b)")
(defmethod.ps+ method2 ((a1 test-type-b) (a2 test-type-b))
  (declare (ignore a1 a2))
  nil "(type-b type-b)")
(defmethod.ps+ method2 ((a1 test-type-a) (a2 test-type-b))
  (declare (ignore a1 a2))
  nil "(type-a type-b)")


(deftest.ps+ for-defmethod.ps+
  (ok (string= (method2 *type-a* 100) "(type-a nil)"))
  (ok (string= (method2 *type-b* 100) "(type-a nil)"))
  (ok (string= (method2 100 *type-b*) "(nil type-b)"))
  (ok (string= (method2 *type-a* *type-b*) "(type-a type-b)"))
  (ok (string= (method2 *type-b* *type-b*) "(type-b type-b)")))

#|
(print (pse:with-use-ps-pack (:this)
         (string= (method2 *type-a* 100) "(type-a nil)")))
|#
