(in-package :cl-user)
(defpackage ps-experiment/defines
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defvar.ps
           :defvar.ps+
           :defsetf.ps
           :defsetf.ps+)
  (:import-from :ps-experiment/base
                :ps.)
  (:import-from :ps-experiment/defines/definer
                :def-ps-definer))
(in-package :ps-experiment/defines)

;; ----- .ps ----- ;;

(def-ps-definer defvar.ps (name initial-value &optional (documentation "")) ()
  `(defvar ,name ,initial-value ,documentation))

(defmacro defsetf.ps (access-fn &rest rest)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ps. (defsetf ,access-fn ,@rest))))

;; ----- .ps+ ----- ;;

(defmacro defvar.ps+ (name initial-value &optional (documentation ""))
  `(progn (defvar.ps ,name ,initial-value ,documentation)
          (defvar ,name ,initial-value ,documentation)))

(defmacro defsetf.ps+ (access-fn &rest rest)
  `(progn (defsetf.ps ,access-fn ,@rest)
          (defsetf ,access-fn ,@rest)))
