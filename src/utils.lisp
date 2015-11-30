(in-package :cl-user)
(defpackage ps-experiment.utils
  (:use :cl
        :cl-ppcre
        :parenscript)
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

;; --- array utils --- ;;

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

;; --- have not classified utils --- ;;

"Limitation: Now, this can judge the type only of objects"
(defpsmacro typep (object type-specifier) 
  (let ((is-symbol (and (listp type-specifier)
                        (= (length type-specifier) 2)
                        (eq (car type-specifier) 'quote)
                        (symbolp (cadr type-specifier)))))
    `(instanceof ,object ,(if is-symbol
                              (eval type-specifier)
                              type-specifier))))

