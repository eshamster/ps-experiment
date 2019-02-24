(defpackage ps-experiment/t/package/utils
  (:use :cl
        :parenscript)
  (:import-from :ps-experiment/package
                :def-ps-definer)
  (:export :defhoge.ps))
(in-package :ps-experiment/t/package/utils)

(def-ps-definer defhoge.ps (name value) ()
  `(defparameter ,name ,value))
