(defpackage ps-experiment/t/package/refer-with-no-importing-export
  (:use :cl
        :ps-experiment/package
        :parenscript
        :ps-experiment/t/package/utils))
(in-package :ps-experiment/t/package/refer-with-no-importing-export)

(defhoge.ps internal-sym 100)
