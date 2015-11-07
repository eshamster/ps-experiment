(in-package :cl-user)
(defpackage ps-experiment.utils
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:shadow :sb-debug
           :var)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree)
  (:import-from :ps-experiment.utils.func
                :defun+ps
                :defun.ps
                :with-use-ps-pack)
  (:export :setf-with
           :load-ps
           :defun+ps
           :defun.ps
           :with-use-ps-pack
           :ps.
           :defmacro.ps))
(in-package :ps-experiment.utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun j.-reader (stream &rest rest)
    (declare (ignore rest))
    (let ((char (read-char stream)))
      (when (or (null char)
                (not (char= char #\.)))
        (error "\".\" is required in the next of \"#j\"")))
    (let (chars)
      (do ((char (read-char stream) (read-char stream)))
          ((char= char #\#))
        (if (upper-case-p char)
            (progn (push #\- chars)
                   (push char chars))
            (push (char-upcase char) chars)))
      (intern (coerce (nreverse chars) 'string))))
  
  (set-dispatch-macro-character #\# #\j #'j.-reader))

(defmacro+ps setf-with (target &body rest)
  (unless (evenp (length rest))
    (error "odd number of args to SETF-WITH"))
  (labels ((extract-slots (result rest)
             (if rest
                 (extract-slots (cons (car rest) result)
                                (cddr rest))
                 (nreverse result))))
    `(with-slots ,(extract-slots nil rest) ,target
       (setf ,@rest))))

(defmacro ps. (&body body)
  `(ps ,@(replace-dot-in-tree body)))

(defmacro defmacro.ps (name args &body body)
  `(defmacro+ps ,name ,args
     ,@(replace-dot-in-tree body)))

(defun make-js-path (name &key (for-load nil))
  (format nil "~Ajs/_~A.js"
          (if for-load "" "static/")
          name))

(defun make-cl-path (name)
  (format nil "static/js/~A.lisp" name))

(defun is-js-older (name)
  (let ((js-path (make-js-path name)))
    (or (not (probe-file js-path))
        (< (file-write-date js-path)
           (file-write-date (make-cl-path name))))))

(defun load-ps (name)
  (print (is-js-older name))
  (if (is-js-older name)
      (with-open-file (out (make-js-path name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format t "(re-)load js: ~A" name)
        (format out (funcall (intern "JS-MAIN"
                                     (string-upcase
                                      (format nil "~A.js.~A"
                                              (car (split "\\."
                                                          (package-name #.*package*)))
                                              name)))))))
  (make-js-path name :for-load t))
