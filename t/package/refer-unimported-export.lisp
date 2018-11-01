(defpackage ps-experiment/t/package/refer-unimported-export
  (:use :cl
        :ps-experiment/package
        :parenscript
        :ps-experiment/t/package/utils)
  (:export :external-sym
           :test-macro))
(in-package :ps-experiment/t/package/refer-unimported-export)

(defhoge.ps external-sym 100)
(defhoge.ps internal-sym 100)

(defpsmacro test-macro ()
  `(+ (external-sym) (internal-sym)))
