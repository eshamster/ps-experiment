(in-package :cl-user)
(defpackage ps-experiment.defines
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defvar.ps)
  (:import-from :alexandria
                :symbolicate)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree)
  (:import-from :ps-experiment.package
                :register-ps-func))
(in-package :ps-experiment.defines)

(defun intern-defvar-func (sym)
  (symbolicate '_defvar_ sym))

(defmacro defvar.ps (name initial-value)
  (let ((register-name (intern-defvar-func name)))
    (register-ps-func register-name)
    `(defun ,register-name ()
       (ps (defvar ,name ,(replace-dot-in-tree initial-value))))))
