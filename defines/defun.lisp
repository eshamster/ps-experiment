(in-package :cl-user)
(defpackage ps-experiment/defines/defun
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defun.ps-only
           :defun.ps
           :defun.ps+)
  (:import-from :alexandria
                :parse-ordinary-lambda-list)
  (:import-from :ps-experiment/defines/definer
                :def-ps-definer))
(in-package :ps-experiment/defines/defun)

(def-ps-definer defun.ps-only (name args &body body) ()
  `(defun ,name ,args ,@body))

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

(defmacro defun.ps+ (name args &body body)
  `(progn (defun.ps-only ,name ,args ,@body)
          (defun ,name ,args ,@body)))
