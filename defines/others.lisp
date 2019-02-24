(in-package :cl-user)
(defpackage ps-experiment/defines/others
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defvar.ps-only
           :defvar.ps
           :defvar.ps+
           :defparameter.ps-only
           :defparameter.ps
           :defparameter.ps+
           :defsetf.ps
           :defsetf.ps+)
  (:import-from :ps-experiment/base
                :ps.)
  (:import-from :ps-experiment/defines/definer
                :def-ps-definer))
(in-package :ps-experiment/defines/others)

;; ----- .ps-only ----- ;;

(def-ps-definer defvar.ps-only (name initial-value &optional (documentation "")) ()
  `(defvar ,name ,initial-value ,documentation))

(def-ps-definer defparameter.ps-only (name initial-value &optional (documentation "")) ()
  `(defparameter ,name ,initial-value ,documentation))

;; ----- .ps ----- ;;

(defmacro defvar.ps (name initial-value &optional (documentation ""))
  `(progn (defvar.ps-only ,name ,initial-value ,documentation)
          (defvar ,name nil ,documentation)))

(defmacro defparameter.ps (name initial-value &optional (documentation ""))
  `(progn (defparameter.ps-only ,name ,initial-value ,documentation)
          (defparameter ,name nil ,documentation)))

(defmacro defsetf.ps (access-fn &rest rest)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ps. (defsetf ,access-fn ,@rest))))

;; ----- .ps+ ----- ;;

(defmacro defvar.ps+ (name initial-value &optional (documentation ""))
  `(progn (defvar.ps-only ,name ,initial-value ,documentation)
          (defvar ,name ,initial-value ,documentation)))

(defmacro defparameter.ps+ (name initial-value &optional (documentation ""))
  `(progn (defparameter.ps-only ,name ,initial-value ,documentation)
          (defparameter ,name ,initial-value ,documentation)))

(defmacro defsetf.ps+ (access-fn &rest rest)
  `(progn (defsetf.ps ,access-fn ,@rest)
          (defsetf ,access-fn ,@rest)))
