(in-package :cl-user)
(defpackage ps-experiment-test.utils
  (:use :cl
        :ps-experiment
        :parenscript
        :prove)
  (:import-from :alexandria
                :with-gensyms))
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

(plan 3)

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
    "Test push"
  (is-list-of.ps+ (let ((x '()))
                    (push 1 x)
                    (push 2 x))
                  '(2 1)))

(subtest
    "Test remove-if"
  (is-list-of.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if (lambda (x) (> x 2)) lst))
                  '(1 2))
  (is-list-of.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if (lambda (x) (> x 2)) lst)
                    lst)
                  '(1 2 3 4)))

(finalize)
