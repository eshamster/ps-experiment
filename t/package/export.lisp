(defpackage ps-experiment/t/package/export
  (:use :cl
        :ps-experiment/package
        :parenscript
        :ps-experiment/t/package/utils)
  (:export :export1
           :export2))
(in-package :ps-experiment/t/package/export)

(defhoge.ps export1 100)
(defhoge.ps export2 200)

(defhoge.ps not-export1 300)
