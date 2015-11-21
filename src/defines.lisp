(in-package :cl-user)
(defpackage ps-experiment.defines
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defvar.ps
           :defstruct.ps)
  (:import-from :alexandria
                :symbolicate)
  (:import-from :ps-experiment.utils.func
                :defun.ps)
  (:import-from :ps-experiment.utils.common 
                :ps.)
  (:import-from :ps-experiment.package
                :register-ps-func))
(in-package :ps-experiment.defines)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun parse-defstruct-name (name)
    (if (symbolp name)
        name
        (error 'type-error :expected-type 'symbol :datum name)))
  
  (defun parse-defstruct-options (options)
    (unless (eq (car options) :include)
      (error "unknown DEFSTRUCT.PS option:~% ~S" options))
    (unless (symbolp (cadr options))
      (error 'type-error :expected-type 'symbol :datum (cadr options)))
    (cadr options))
  
  (defun parse-defstruct-name-and-options (name-and-options)
    (if (listp name-and-options)
        (values (parse-defstruct-name (car name-and-options))
                (parse-defstruct-options (cadr name-and-options)))
        (values (parse-defstruct-name name-and-options) nil))))

(defmacro defvar.ps (name initial-value)
  (let ((register-name (symbolicate '_defvar_ name)))
    (register-ps-func register-name)
    `(defun ,register-name ()
       (ps. (defvar ,name ,initial-value)))))

;; We refered goog.inherits for the inheritance code
;; https://github.com/google/closure-library/blob/master/closure/goog/base.js#L2170
(defmacro defstruct.ps (name-and-options &rest slot-description)
  "This is the tiny subset of defsturt in terms of syntax.
    name-and-options::= structure-name | (structure-name (:include included-structure-name))
    slot-description::= slot-name | (slot-name slot-init-form)"
  (multiple-value-bind (name parent)
      (parse-defstruct-name-and-options name-and-options)
    `(progn
       ,(let ((register-name (symbolicate '_defstruct_ name)))
          (register-ps-func register-name)
          `(defun ,register-name ()
             (ps.
               (defun ,name ()
                 ,(when parent
                        `((@ ,parent call) this))
                 ,@(mapcar (lambda (elem)
                             (if (consp elem)
                                 `(setf (@ this ,(car elem)) ,(cadr elem))
                                 `(setf (@ this ,elem) nil)))
                           slot-description)))))
       (defun.ps ,(symbolicate name '-p) (obj)
         (instanceof obj ,name))
       ,(when parent
              (let ((inherit-func (symbolicate '_inheritance_ name)))
                (register-ps-func inherit-func)
                `(defun ,inherit-func ()
                   (ps (funcall (lambda ()
                                  (defun temp-ctor ())
                                  (setf (@ temp-ctor prototype) (@ ,parent prototype))
                                  (setf (@ ,name super-class_) (@ ,parent prototype))
                                  (setf (@ ,name prototype) (new (temp-ctor)))
                                  (setf (@ ,name prototype constructor) ,name)))))))
       '(:struct ,name))))
