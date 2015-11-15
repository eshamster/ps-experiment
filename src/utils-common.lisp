(in-package :cl-user)
(defpackage ps-experiment.utils.common
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:shadow :sb-debug
           :var)
  (:export :replace-dot-in-tree))
(in-package :ps-experiment.utils.common)

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
