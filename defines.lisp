(in-package :cl-user)
(defpackage ps-experiment/defines
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defvar.ps
           :defvar.ps+
           :defun.ps
           :defun.ps+
           :defstruct.ps
           :defstruct.ps+)
  (:import-from :anaphora
                :aif
                :sif
                :it)
  (:import-from :alexandria
                :symbolicate
                :parse-ordinary-lambda-list)
  (:import-from :ps-experiment/base
                :ps.
                :defmacro.ps)
  (:import-from :ps-experiment/package
                :make-ps-definer
                :def-ps-definer
                :def-top-level-form.ps
                :defun.ps-only
                :register-ps-type
                :add-unintern-all-ps-symbol-hook))
(in-package :ps-experiment/defines)

;; ----- .ps ----- ;;

(defun extract-arg-names (lambda-list)
  (multiple-value-bind (required optional rest
                        keys allow-other-keys aux keyp)
      (parse-ordinary-lambda-list lambda-list
                                  :normalize nil)
    (declare (ignore allow-other-keys keyp))
    (labels ((make-a-list (got)
               (if (listp got)
                   (mapcar (lambda (elem)
                             (if (atom elem) elem (car elem)))
                           got)
                   (list got))))
      (mapcan #'make-a-list
              (list required optional rest keys aux)))))

(def-ps-definer defun.ps (name args &body body)
    (:before `(defun ,name ,args
                (declare ,(cons 'ignore
                                (extract-arg-names args)))
                (error (format nil "~A is only defined but not implemented as a CL function"
                               ',name))))
  `(defun ,name ,args ,@body))

(def-ps-definer defvar.ps (name initial-value &optional (documentation "")) ()
  `(defvar ,name ,initial-value ,documentation))

;; ----- .ps+ ----- ;;

(defmacro defun.ps+ (name args &body body)
  `(progn (defun.ps-only ,name ,args ,@body)
          (defun ,name ,args ,@body)))

(defmacro defvar.ps+ (name initial-value &optional (documentation ""))
  `(progn (defvar.ps ,name ,initial-value ,documentation)
          (defvar ,name ,initial-value ,documentation)))

(defmacro defstruct.ps+ (name-and-options &rest slot-description)
  `(progn (defstruct.ps-impl (t) ,name-and-options ,@slot-description)
          (defstruct ,name-and-options ,@slot-description)))

;; ----- defstruct ----- ;;

(defun parse-defstruct-name (name)
  (if (symbolp name)
      name
      (error 'type-error :expected-type 'symbol :datum name)))
  
(defun parse-defstruct-include-option (options)
  (unless (eq (car options) :include)
    (error "unknown DEFSTRUCT.PS option:~% ~S" options))
  (unless (symbolp (cadr options))
    (error 'type-error :expected-type 'symbol :datum (cadr options)))
  (cdr options))
  
(defun parse-defstruct-name-and-options (name-and-options)
  (if (listp name-and-options)
      (values (parse-defstruct-name (car name-and-options))
              (parse-defstruct-include-option (cadr name-and-options)))
      (values (parse-defstruct-name name-and-options) nil)))

(defun parse-defstruct-slot-description (slot-description)
  (let ((result (mapcar (lambda (slot)
                          (if (consp slot)
                              slot
                              (list slot nil)))
                        slot-description)))
    (if (every (lambda (slot) (symbolp (car slot))) result)
        result
        (error 'type-error :expected-type 'symbol :datum slot-description))))

(defvar *ps-struct-slots* (make-hash-table)
  "Store slots of each structure made by defstruct.ps
key = structure-name
value = ({(slot-name slot-init-form}*)")

(add-unintern-all-ps-symbol-hook
 (lambda () (setf *ps-struct-slots* (make-hash-table))))
  
(defun find-defstruct-slots (parent)
  (aif (gethash parent *ps-struct-slots*)
       it
       (error 'unbound-variable :name parent)))

;; Note: Ex. parent-info := (name (slot1 value1) (slot2 value2))
(defun modify-parent-slots (slots parent-info)
  (let ((modified-slots nil))
    (dolist (slot slots)
      (push (list (car slot) (cadr slot)) modified-slots))
    (setf modified-slots (nreverse modified-slots))
    (dolist (mod-slot (cdr parent-info))
      (unless (and (listp mod-slot) (= (length mod-slot) 2))
        (error "~A is invalid slot-description" mod-slot))
      (sif (find-if (lambda (slot)
                      (equal (symbol-name (car slot))
                             (symbol-name (car mod-slot))))
                    modified-slots)
           (setf (cadr it) (cadr mod-slot))
           (error "~A doesn't have the slot ~A" (car parent-info) (car mod-slot))))
    modified-slots))

(defun check-slot-name-duplication (slots)
  (unless (= (length slots)
             (length (remove-duplicates slots
                                        :test (lambda (a b) (eql (car a) (car b))))))
    (error "duplicate slot name")))

(defun merge-defstruct-slots (parent-info slots)
  (if (null parent-info)
      slots
      (let ((merged-slots (append (modify-parent-slots
                                   (find-defstruct-slots (car parent-info))
                                   parent-info)
                                  slots)))
        merged-slots)))

(defun register-defstruct-slots (name slots)
  (setf (gethash name *ps-struct-slots*) slots))

(defmacro defstruct.ps (name-and-options &rest slot-description)
  `(defstruct.ps-impl (nil) ,name-and-options ,@slot-description))

(defmacro defun-wrapper (ps-only-p name arg-list &body body)
  (if ps-only-p
      `(defun.ps-only ,name ,arg-list ,@body)
      `(defun.ps ,name ,arg-list ,@body)))

;; We refered goog.inherits for the inheritance code
;; https://github.com/google/closure-library/blob/master/closure/goog/base.js#L2170
(defmacro defstruct.ps-impl ((ps-only-p) name-and-options &rest slot-description)
  "This is the tiny subset of defsturt in terms of syntax.
    name-and-options::= structure-name | (structure-name (:include included-structure-name {inherit-slot-description}*))
    slot-description::= slot-name | (slot-name slot-init-form)
    inherit-slot-description::= (slot-name slot-init-form)

    included-structure-name---a symbol.
    structure-name---a symbol.
    slot-name---a symbol.
    slot-init-form---a form."
  (bind:bind (((:values name parent-info)
               (parse-defstruct-name-and-options name-and-options))
              (parent (car parent-info))
              (slots
               (parse-defstruct-slot-description slot-description)))
    (setf slots (merge-defstruct-slots parent-info slots))
    (check-slot-name-duplication slots)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-defstruct-slots ',name ',slots)
       (defun-wrapper ,ps-only-p ,name ()
         ,@(mapcar (lambda (slot)
                     `(setf (@ this ,(car slot)) ,(cadr slot)))
                   slots)
         this)
       (register-ps-type ',name)
       (defun-wrapper ,ps-only-p ,(symbolicate 'make- name) (&key ,@slots)
         (let ((result (new (,name))))
           ,@(mapcar (lambda (elem)
                       `(setf (@ result ,(car elem)) ,(car elem)))
                     slots)
           result))
       ,@(mapcar (lambda (slot)
                   `(defmacro.ps ,(symbolicate name '- (car slot)) (obj)
                      `(@ ,obj ,',(car slot))))
                 slots)
       (defun-wrapper ,ps-only-p ,(symbolicate name '-p) (obj)
         (instanceof obj ,name))
       ,(when parent
              `(def-top-level-form.ps ,(symbolicate '_defstruct-inherit_ name)
                 (funcall (lambda ()
                            (labels ((temp-ctor ()))
                              (setf (@ temp-ctor prototype) (@ ,parent prototype))
                              (setf (@ ,name super-class_) (@ ,parent prototype))
                              (setf (@ ,name prototype) (new (temp-ctor)))
                              (setf (@ ,name prototype constructor) ,name))))))
       ',name)))
