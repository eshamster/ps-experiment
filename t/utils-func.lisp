(in-package :cl-user)
(defpackage ps-experiment-test.utils.func
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment-test.test-utils
        :prove)
  (:import-from :ps-experiment
                :defmacro.ps
                :with-use-ps-pack)
  (:import-from :ps-experiment.package
                :unintern-all-ps-symbol))
(in-package :ps-experiment-test.utils.func)

;; --- prepare ---

(defun.ps position ()
  (setf this.x 20)
  (setf this.y 30))

(defun.ps add-pos (a b)
  (let ((c (new (position))))
    (setf c.x (+ a.x b.x))
    (setf c.y (+ a.y b.y))
    c))

(defun.ps sub-pos (a b)
  (let ((c (new (position))))
    (setf c.x (- a.x b.x))
    (setf c.y (- a.y b.y))
    c))

;; --- body --- 

(plan 1)

(subtest
    "Test defun.ps"
  (is (execute-js (with-use-ps-pack (:this)
                    (let ((pos-a (new (position)))
                          (pos-b (new (position)))
                          (pos-c (new (position))))
                      (setf pos-a.x 100)
                      (setf pos-b.x 50)
                      (setf pos-c.x 80)
                      (@ (sub-pos (add-pos pos-a
                                           pos-b)
                                  pos-c)
                         x))))
      70)
  (is-error (execute-js (with-use-ps-pack (:this)
                          (let ((pos-a (new (position)))
                                (pos-b (new (position))))
                            (mult-pos pos-a pos-b))))
            'undefined-variable))

(finalize)

(unintern-all-ps-symbol)
