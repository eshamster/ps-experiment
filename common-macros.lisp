(in-package :cl-user)
(defpackage ps-experiment/common-macros
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :setf-with
           :with-slots-pair)
  (:import-from :ps-experiment/base
                :defmacro.ps+))
(in-package :ps-experiment/common-macros)

;; * These macros are not included in Common Lisp but for convinence.
;; * Now, they are not exported from the ps-experiment package.

(defmacro.ps+ setf-with (target &body rest)
  (unless (evenp (length rest))
    (error "odd number of args to SETF-WITH"))
  (labels ((extract-slots (result rest)
             (if rest
                 (extract-slots (cons (car rest) result)
                                (cddr rest))
                 (nreverse result))))
    `(with-slots ,(extract-slots nil rest) ,target
       (setf ,@rest))))

(defmacro.ps+ with-slots-pair (pair &body body)
  (unless (evenp (length pair))
    (error "with-slots-pair needs an even number length list as a first argument"))
  (labels ((rec (rest-pair)
             (if rest-pair
                 `((with-slots ,(car rest-pair) ,(cadr rest-pair)
                     ,@(rec (cddr rest-pair))))
                 body)))
    (car (rec pair))))
