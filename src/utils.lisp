(in-package :cl-user)
(defpackage ps-experiment.utils
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree)
  (:import-from :ps-experiment.utils.func
                :defun+ps
                :defun.ps)
  (:export :setf-with
           :defun+ps
           :defun.ps
           :ps.
           :defmacro.ps))
(in-package :ps-experiment.utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun j.-reader (stream &rest rest)
    (declare (ignore rest))
    (let ((char (read-char stream)))
      (when (or (null char)
                (not (char= char #\.)))
        (error "\".\" is required in the next of \"#j\"")))
    (let (chars)
      (do ((char (read-char stream) (read-char stream)))
          ((char= char #\#))
        (if (upper-case-p char)
            (progn (push #\- chars)
                   (push char chars))
            (push (char-upcase char) chars)))
      (intern (coerce (nreverse chars) 'string))))
  
  (set-dispatch-macro-character #\# #\j #'j.-reader))

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

(defmacro ps. (&body body)
  `(ps ,@(replace-dot-in-tree body)))

(defmacro defmacro.ps (name args &body body)
  `(defmacro+ps ,name ,args
     ,@(replace-dot-in-tree body)))
