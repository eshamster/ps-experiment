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

(defmacro def-test-package (name)
  `(defpackage ,name
     (:use :cl
           :ps-experiment
           :parenscript)))

(def-test-package test.func.pack-a)
(def-test-package test.func.pack-b)


; --- prepare ---

(in-package :test.func.pack-a)

(defun.ps position ()
  (setf this.x 20)
  (setf this.y 30))

(in-package :test.func.pack-b)


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

; --- body --- 

(in-package :ps-experiment-test.utils.func)

(plan 1)

(defmacro.ps test-js-program ()
  `(let ((pos-a (new (position)))
         (pos-b (new (position)))
         (pos-c (new (position))))
     (setf pos-a.x 100)
     (setf pos-b.x 50)
     (setf pos-c.x 80)
     (@ (sub-pos (add-pos pos-a
                          pos-b)
                 pos-c)
        x)))

(subtest
    "Test defun.ps"
  (is (execute-js (with-use-ps-pack (:test.func.pack-a
                                     :test.func.pack-b)
                    (test-js-program)))
      70)
  (is-error (execute-js (with-use-ps-pack (:test.func.pack-a)
                          (test-js-program)))
            'undefined-variable))

(finalize)

(unintern-all-ps-symbol)
