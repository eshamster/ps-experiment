(in-package :cl-user)
(defpackage ps-experiment.utils
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :setf-with)
  (:import-from :ps-experiment.base
                :defmacro.ps+))
(in-package :ps-experiment.utils)

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

(defun quoted-symbolp (object)
  (and (listp object)
       (= (length object) 2)
       (eq (car object) 'quote)
       (symbolp (cadr object))))

;; --- array utils --- ;;

(defpsmacro nth (n list)
  `(aref ,list ,n))

(defpsmacro push (item place)
  `(progn ((@ ,place unshift) ,item)
          ,place))

(defpsmacro every (predicate sequence)
  `((@ ,sequence every) ,predicate))

(defpsmacro some (predicate sequence)
  `((@ ,sequence some) ,predicate))

(defpsmacro remove-if (test sequence)
  (with-ps-gensyms (copy)
    `(let ((,copy ,sequence))
       ((@ ,copy filter) (lambda (x) (not (funcall ,test x)))))))

(defpsmacro remove-if-not (test sequence)
  (with-ps-gensyms (copy)
    `(let ((,copy ,sequence))
       ((@ ,copy filter) ,test))))

;; --- hash utils --- ;;

(defpsmacro make-hash-table ()
  `(@ {}))

;; TODO: Support &optional default
(defpsmacro gethash (key hash-table)
  (if (quoted-symbolp key)
      `(aref ,hash-table ,(string (cadr key)))
      `(aref ,hash-table ,key)))

;; Limitation: Even if keys are registered as number,
;;             it is interpreted as string.
(defpsmacro maphash (func hash-table)
  (ps-once-only (hash-table)
    `(dolist (key ((@ -object keys) ,hash-table))
       (funcall ,func key (gethash key ,hash-table)))))

;; --- have not classified utils --- ;;

"Limitation: Now, this can judge the type only of objects"
(defpsmacro typep (object type-specifier) 
  (let ((is-symbol (quoted-symbolp type-specifier)))
    (if is-symbol
        `(instanceof ,object ,(cadr type-specifier))
        `(instanceof ,object (if (stringp ,type-specifier)
                                 (eval ,type-specifier)
                                 ,type-specifier)))))

(defpsmacro error (datum &rest args)
  (if (null args)
      `(throw ,datum)
      `(throw ,(format nil "~A: ~A" datum args))))
