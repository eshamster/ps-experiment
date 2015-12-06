(in-package :cl-user)
(defpackage ps-experiment-test.utils
  (:use :cl
        :ps-experiment
        :ps-experiment-test.test-utils
        :parenscript
        :prove)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :ps-experiment.package
                :unintern-all-ps-symbol))
(in-package :ps-experiment-test.utils)

(defun js-array-to-list (js-array)
  (let ((result nil))
    (dotimes (i (cl-js:js-array-length js-array))
      (push (cl-js:js-aref js-array i) result))
    (nreverse result)))

(defmacro is-list-of.ps+ (got expected)
  (if (not (listp expected))
      (error 'type-error :expected-type 'list :datum expected))
  (with-gensyms (js-got js-expected)
    `(progn
       (print "Common Lisp: ")
       (is ,got ,expected :test #'equalp)
       (print "JavaScript: ")
       (let ((,js-got (cl-js:run-js (ps. ,got)))
             (,js-expected (cl-js:run-js (ps. ,expected))))
         (is ,js-got ,js-expected :test #'equalp
             (format nil "~A is expected (got: ~A)"
                     (js-array-to-list ,js-expected)
                     (js-array-to-list ,js-got)))))))

(plan 10)

(subtest
    "Test setf-with"
  (is-expand (setf-with obj
               x 100
               y 200
               z 300)
             (with-slots (x y z) obj
               (setf x 100
                     y 200
                     z 300))))

(subtest
    "Test nth"
  (prove-in-both (is (nth 2 '(1 2 3 4)) 3))
  (prove-in-both (ok (null (nth 2 '(1 2))))))

(subtest
    "Test push"
  (is-list-of.ps+ (let ((x '()))
                    (push 1 x)
                    (push 2 x))
                  '(2 1)))

(subtest
    "Test every"
  (prove-in-both (ok (every (lambda (x) (> x 2)) '(3 4 5))))
  (prove-in-both (ok (not (every (lambda (x) (> x 2)) '(2 3 4))))))

(subtest
    "Test some"
  (prove-in-both (ok (some (lambda (x) (< x 2)) '(2 1 3))))
  (prove-in-both (ok (not (some (lambda (x) (< x 2)) '(2 3 4))))))

(subtest
    "Test remove-if"
  (is-list-of.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if (lambda (x) (> x 2)) lst))
                  '(1 2))
  (is-list-of.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if (lambda (x) (> x 2)) lst)
                    lst)
                  '(1 2 3 4)))

(subtest
    "Test remove-if-not"
  (is-list-of.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if-not (lambda (x) (> x 2)) lst))
                  '(3 4))
  (is-list-of.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if-not (lambda (x) (> x 2)) lst)
                    lst)
                  '(1 2 3 4)))

(subtest
    "Test hash table"
  ;;  (is (ps (make-hash-table)) "{};" :test #'equal)
  (is (ps (gethash key tbl)) "tbl[key];" :test #'equal)
  (is (ps (gethash 'key tbl)) "tbl['KEY'];" :test #'equal)
  (is (ps (gethash 0 tbl)) "tbl[0];" :test #'equal)
  (is (ps (gethash (+ 1 2) tbl)) "tbl[1 + 2];" :test #'equal)
  (prove-in-both (is (let ((tbl (make-hash-table)))
                       (setf (gethash 'x tbl) 100)
                       (gethash 'x tbl))
                     100))
  (subtest
      "Test maphash"
    (prove-in-both (is (let ((tbl (make-hash-table))
                             (sum-key 0)
                             (sum-value 0))
                         (setf (gethash 10 tbl) 100)
                         (setf (gethash 20 tbl) 200)
                         (setf (gethash 30 tbl) 300)
                         (maphash (lambda (k v)
                                    (incf sum-key k)
                                    (incf sum-value v))
                                  tbl)
                         ;;(+ sum-key sum-value)
                         sum-value)
                       600)
                   :prints-js t)))

(subtest
    "Test error"
  (prove-in-both (is-error (error 'simple-error)
                           'simple-error))
  (prove-in-both (is-error (error 'type-error :expected-type 'fixnum :datum "abc")
                           'type-error)))

;; --- affect global env --- ;;
(defstruct.ps+ test1 a)
(defstruct.ps+ (test2 (:include test1)) b)
(defstruct.ps+ test3 a)

(subtest
    "Test typep"
  (prove-in-both (ok (typep (make-test1) 'test1)))
  (prove-in-both (ok (typep (make-test2) 'test1)))
  (prove-in-both (ok (not (typep (make-test1) 'test3))))
  (prove-in-both (ok (let ((type 'test1))
                       (typep (make-test1) type)))))

(unintern-all-ps-symbol)

(finalize)
