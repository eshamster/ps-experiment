(in-package :cl-user)
(defpackage ps-experiment.sample-pack
  (:use :cl
        :parenscript)
  (:import-from :ps-experiment.utils
                :defun.ps
                :with-use-ps-pack))
(in-package :ps-experiment.sample-pack)

(defun.ps fs1 (x)
  (* x 2))

(defun.ps fs2 (y)
  (+ (fs1 y) 100))
