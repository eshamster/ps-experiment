(in-package :cl-user)
(defpackage ps-experiment 
  (:nicknames :pse)
  (:import-from :ps-experiment.ps-macros-for-compatibility)
  (:import-from :ps-experiment.base
                :ps.
                :defmacro.ps
                :defmacro.ps+
                :enable-ps-experiment-syntax
                :--)
  (:import-from :ps-experiment.package
                :with-use-ps-pack
                :def-top-level-form.ps
                :defun.ps-only)
  (:import-from :ps-experiment.defines
                :defvar.ps
                :defvar.ps+
                :defun.ps
                :defun.ps+
                :defstruct.ps
                :defstruct.ps+)
  (:export :defvar.ps
           :defvar.ps+
           :defun.ps-only
           :defun.ps
           :defun.ps+
           :defstruct.ps
           :defstruct.ps+
           :with-use-ps-pack
           :ps.
           :defmacro.ps
           :defmacro.ps+
           :def-top-level-form.ps
           :enable-ps-experiment-syntax
           :--))
