(defpackage ps-experiment/t/package/type-specifier
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment/t/package/utils)
  (:export :test-type)
  (:import-from :ps-experiment/package
                :register-ps-type))
(in-package :ps-experiment/t/package/type-specifier)

(register-ps-type 'test-type)
