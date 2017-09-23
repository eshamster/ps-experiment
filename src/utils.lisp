(in-package :cl-user)
(defpackage ps-experiment.utils
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:import-from :ps-experiment.base
                :defmacro.ps+))
(in-package :ps-experiment.utils)

(defun quoted-symbolp (object)
  (and (listp object)
       (= (length object) 2)
       (eq (car object) 'quote)
       (symbolp (cadr object))))

;; --- array utils --- ;;

;; c[ad]{1-2}r
;; Limitaton: cd[ad]*r cannot be used for setting

(defpsmacro car (x)
  `(@ ,x 0))
(defpsmacro cdr (x)
  (ps-once-only (x)
    `((@ ,x slice) 1 (@ ,x length))))
(defpsmacro caar (x)
  `(@ ,x 0 0))
(defpsmacro cadr (x)
  `(@ ,x 1))
(defpsmacro cdar (x)
  `(cdr (@ ,x 0)))
(defpsmacro cddr (x)
  `(cdr (cdr ,x)))

(defpsmacro nth (n list)
  `(aref ,list ,n))

(defpsmacro listp (object)
  (if (null object)
      t
      `(instanceof ,object -array)))

(defpsmacro atom (object)
  (if (or (null object)
          (and (listp object)
               (= (length object) 2)
               (eq (car object) 'quote)
               (null (cadr object))))
      t
      `(not (listp ,object))))

(defpsmacro push (item place)
  `(progn ((@ ,place unshift) ,item)
          ,place))

(defpsmacro pushnew (item place &key test)
  `(progn (unless (find-if (lambda (elem)
                             ,(if test
                                  `(funcall ,test ,item elem)
                                  `(eq ,item elem)))
                           ,place)
            (push ,item ,place))
          ,place))

;; TODO: Throw error if the range beyond the sequence.
(defpsmacro subseq (sequence start &optional end)
  (if end
      `((@ ,sequence slice) ,start ,end)
      `((@ ,sequence slice) ,start)))

(defpsmacro every (predicate sequence)
  `((@ ,sequence every) ,predicate))

(defpsmacro some (predicate sequence)
  `((@ ,sequence some) ,predicate))

(defpsmacro find-if (predicate sequence)
  (with-ps-gensyms (x)
    `(dolist (,x ,sequence)
       (when (funcall ,predicate ,x)
         (return ,x)))))

(defpsmacro find (item sequence)
  (with-ps-gensyms (target)
    `(find-if (lambda (,target) (eq ,item ,target)) ,sequence)))

;; Note: This doesn't support some builtin functions such as min
;; because, for example, #'min is not automatically interpreted
;; as Math.min in Parenscript.
(defpsmacro reduce (function sequence)
  `((@ ,sequence reduce) ,function))

(defpsmacro remove-if (test sequence)
  (with-ps-gensyms (copy)
    `(let ((,copy ,sequence))
       ((@ ,copy filter) (lambda (x) (not (funcall ,test x)))))))

(defpsmacro remove (item sequence)
  (with-ps-gensyms (target)
    `(remove-if (lambda (,target) (eq ,item ,target)) ,sequence)))

(defpsmacro remove-if-not (test sequence)
  (with-ps-gensyms (copy)
    `(let ((,copy ,sequence))
       ((@ ,copy filter) ,test))))

(defpsmacro nreverse (sequence)
  `((@ ,sequence reverse)))

(defpsmacro reverse (sequence)
  (with-ps-gensyms (copy)
    `(let ((,copy ((@ ,sequence concat))))
       (nreverse ,copy))))

;; TODO: Implement as (function list &rest rest-lists)
(defpsmacro mapcar (function list)
  `((@ ,list map) ,function))

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

;; --- very simple format --- ;;

;; The first purpose of this tiny "format" is to create a common interface
;; with CL to output string with arguments.
;; If required or if I feel like it, it will be enhanced.

(defun interpret-ps-format (control-string arg-list)
  (unless arg-list
    (return-from interpret-ps-format control-string))
  (let ((result `(,control-string +)))
    (dolist (arg arg-list)
      (push "; " result)
      (push arg result))
    (reverse result)))

(defpsmacro format (stream control-string &rest args)
  (let ((temp (interpret-ps-format control-string args)))
    (if stream
        `(print ,temp)
        temp)))

;; --- type utils --- ;;

"Limitation: Now, this can judge the type only of objects"
(defpsmacro typep (object type-specifier) 
  (if (quoted-symbolp type-specifier)
      `(typep ,object ,(cadr type-specifier))
      `(instanceof ,object (if (stringp ,type-specifier)
                               (eval ,type-specifier)
                               ,type-specifier))))

(defpsmacro typecase (keyform &body clauses)
  (with-ps-gensyms (g-keyform)
    `(let ((,g-keyform ,keyform))
       (cond ,@(mapcar (lambda (clause)
                         (if (eq (car clause) t)
                             `(t ,@(cdr clause))
                             `((typep ,g-keyform ,(car clause)) ,@(cdr clause))))
                      clauses)))))

(defpsmacro etypecase (keyform &body clauses)
  `(typecase ,keyform
     ,@clauses
     (t (error 'type-error
               ,(format nil "The place is '~A'. The expected type is '~A'"
                        keyform (cons :or (mapcar #'car clauses)))))))

(defpsmacro check-type (place type-specifier)
  `(unless ,(if (string= (symbol-name type-specifier) "STRING")
                `(stringp ,place)
                `(typep ,place ,type-specifier))
       (error 'type-error
              ,(format nil "The place is '~A'. The expected type is '~A'"
                       place type-specifier))))

;; --- have not classified utils --- ;;

(defpsmacro error (datum &rest args)
  (cond ((null args) `(throw ,datum))
        ((stringp datum) `(throw ,(eval `(format nil ,datum
                                                 ,@(mapcar (lambda (arg)
                                                             (format nil "~A" arg))
                                                           args)))))
        (t `(throw ,(format nil "~A: ~A" datum args)))))

