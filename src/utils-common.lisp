(in-package :cl-user)
(defpackage ps-experiment.utils.common
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :replace-dot-in-tree
           :ps.
           :defmacro.ps))
(in-package :ps-experiment.utils.common)


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
  `(defmacro+ps ,name ,args
     ,@(replace-dot-in-tree body)))
