(in-package :cl-user)
(defpackage ps-experiment.utils
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:import-from :ps-experiment.utils.common)
  (:import-from :ps-experiment.utils.func
                :defun.ps)
  (:export :setf-with))
(in-package :ps-experiment.utils)

(defmacro+ps setf-with (target &body rest)
  (unless (evenp (length rest))
    (error "odd number of args to SETF-WITH"))
  (labels ((extract-slots (result rest)
             (if rest
                 (extract-slots (cons (car rest) result)
                                (cddr rest))
                 (nreverse result))))
    `(with-slots ,(extract-slots nil rest) ,target
       (setf ,@rest))))

(defpsmacro push (item place)
  `((@ ,place push) ,item))
