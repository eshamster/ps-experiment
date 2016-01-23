(in-package :cl-user)
(defpackage ps-experiment.base
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :replace-dot-in-tree
           :ps.
           :defmacro.ps
           :defmacro.ps+
           :enable-ps-experiment-syntax
           :--))
(in-package :ps-experiment.base)

(defpsmacro -- (&rest rest)
  `(chain ,@rest))

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

(defmacro enable-ps-experiment-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (set-dispatch-macro-character #\# #\j #'j.-reader)))

(defun replace-dot-sep (elem)
  (if (and (symbolp elem)
           (not (null (symbol-package elem)))) ; gensym case
      (let ((name (symbol-name elem))
            (pack-name (package-name (symbol-package elem))))
        (cond ((and (> (length name) 1)
                    (string= name "!!" :start1 0 :end1 2))
               (intern (subseq name 2) pack-name))
              ((ppcre:scan "\\." name)
               `(@ ,@(mapcar (lambda (x) (intern x pack-name))
                                (ppcre:split "\\." name))))
              (t elem)))
      elem))

(defun replace-dot-in-tree (tree)
  (labels ((rec (rest)
             (cond
               ((consp rest) (let (result)
                               (dolist (elem rest)
                                 (push (rec elem) result))
                               (nreverse result)))
               (rest (replace-dot-sep rest)))))
    (rec tree)))

(defmacro ps. (&body body)
  `(ps ,@(replace-dot-in-tree body)))

(defmacro defmacro.ps (name args &body body)
  "Note: 2015/12/12
The eval-when in defpsmacro has been added to the master branch of Parenscript repository.
However, the version has not been registered into quicklisp"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defpsmacro ,name ,args ,@(replace-dot-in-tree body))))

(defmacro defmacro.ps+ (name args &body body)
  `(progn (defmacro.ps ,name ,args ,@body)
          (defmacro ,name ,args ,@body)))
