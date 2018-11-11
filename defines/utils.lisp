(in-package :cl-user)
(defpackage ps-experiment/defines/utils
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :extract-arg-names)
  (:import-from :alexandria
                :parse-ordinary-lambda-list))
(in-package :ps-experiment/defines/utils)

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
