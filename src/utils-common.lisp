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
  (if (symbolp elem)
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
             (let (result)
               (when rest
                 (dolist (elem rest)
                   (push (if (listp elem)
                             (rec elem)
                             (replace-dot-sep elem))
                         result)))
               (nreverse result))))
    (rec tree)))
