(in-package :cl-user)
(defpackage ps-experiment/defines/defmethod
  (:use :cl
        :cl-ppcre
        :parenscript
        :ps-experiment/ps-macros-for-compatibility)
  (:export :defgeneric.ps-only
           :defgeneric.ps
           :defgeneric.ps+
           :defmethod.ps-only
           :defmethod.ps
           :defmethod.ps+)
  (:import-from :alexandria
                :make-keyword
                :parse-ordinary-lambda-list
                :symbolicate)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :ps-experiment/defines/defun
                :defun.ps-only
                :defun.ps
                :defun.ps+)
  (:import-from :ps-experiment/defines/defstruct
                :defstruct.ps+)
  (:import-from :ps-experiment/defines/others
                :defvar.ps+)
  (:import-from :ps-experiment/defines/utils
                :extract-arg-names)
  (:import-from :ps-experiment/package
                :def-top-level-form.ps))
(in-package :ps-experiment/defines/defmethod)

;; --- data structures --- ;;

(defstruct.ps+ dispatch-item type-list func)

(defstruct defgeneric-options (documentation ""))

;; TODO: clean the table
(defvar.ps+ *dispatch-list-table* (make-hash-table)
  "Key: method name, Value: sorted list of dispatch-item")

;; --- interfaces --- ;;

;; - defgeneric - ;;

(defmacro defgeneric.ps-only (function-name gf-lambda-list &rest options)
  (let ((opt (parse-defgeneric-options options)))
    `(progn
       (def-top-level-form.ps ,(symbolicate '_defgeneric_form_ function-name)
         (setf (gethash ,(make-table-key function-name) *dispatch-list-table*)
               (list)))
       (defun.ps-only ,function-name ,gf-lambda-list
         ,(defgeneric-options-documentation opt)
         (funcall (find-dispatch-func ,(make-table-key function-name)
                                      (list ,@(extract-dispatch-instances gf-lambda-list)))
                  ,@(convert-gf-lambda-list-to-call-list gf-lambda-list))))))

(defmacro defgeneric.ps (function-name gf-lambda-list &rest options)
  `(progn (defgeneric.ps-only ,function-name ,gf-lambda-list ,@options)
          (defgeneric ,function-name ,gf-lambda-list ,@options)))

(defmacro defgeneric.ps+ (function-name gf-lambda-list &rest options)
  `(defgeneric.ps ,function-name ,gf-lambda-list ,@options))

;; - defmethod - ;;

(defmacro defmethod.ps-only (function-name specialized-lambda-list &body body)
  (let ((dispatch-types (extract-dispatch-types specialized-lambda-list)))
    `(def-top-level-form.ps ,(symbolicate '_defmethod_form_ function-name
                                          (format nil "_(~{~A~^ ~})" dispatch-types))
       (push-dispatch-func ,(make-table-key function-name)
                           ',dispatch-types
                           (lambda ,(convert-specialized-lambda-list-to-lambda-list
                                     specialized-lambda-list)
                             ,@body)))))

(defmacro defmethod.ps (function-name specialized-lambda-list &body body)
  (let ((args (mapcar (lambda (elem)
                        (if (listp elem) (car elem) elem))
                      specialized-lambda-list)))
    `(progn (defmethod.ps-only ,function-name ,specialized-lambda-list ,@body)
            (defmethod ,function-name ,specialized-lambda-list
              (declare ,(cons 'ignore
                              (extract-arg-names args)))
              (error (format nil "~A for ~A is only defined but not implemented as a CL method"
                             ',function-name
                             ',(extract-dispatch-types specialized-lambda-list)))))))

(defmacro defmethod.ps+ (function-name specialized-lambda-list &body body)
  `(progn (defmethod.ps-only ,function-name ,specialized-lambda-list ,@body)
          (defmethod ,function-name ,specialized-lambda-list ,@body)))

;; --- aux functions --- ;;

(eval-when (:load-toplevel :execute :compile-toplevel)
  ;; - about lambda-list - ;;
  (defun extract-dispatch-pair (lambda-list)
    "Ex. (a (b type-b) &key c) -> ((a nil) (b type-b))"
    (labels ((rec (rest result)
               (let ((head (car rest)))
                 (if (or (not rest)
                         (and (symbolp head)
                              (string= (subseq (symbol-name head) 0 1)
                                       "&")))
                     (nreverse result)
                     (rec (cdr rest)
                          (cons (if (listp head)
                                    head
                                    (list head))
                                result))))))
      (rec lambda-list nil)))

  (defun extract-dispatch-instances (lambda-list)
    "Ex. (a (b type-b)) -> (a b)"
    (mapcar #'car (extract-dispatch-pair lambda-list)))

  (defun extract-dispatch-types (lambda-list)
    "Ex. (a (b type-b)) -> (nil type-b)"
    (mapcar #'cadr (extract-dispatch-pair lambda-list)))

  (defun convert-gf-lambda-list-to-call-list (gf-lambda-list)
    "Ex. (a b &key c) -> (a b :c c)"
    (multiple-value-bind (required optional rest
                                   keys allow-other-keys aux keyp)
        (parse-ordinary-lambda-list gf-lambda-list
                                    :normalize nil)
      (declare (ignore allow-other-keys aux keyp))
      (labels ((make-a-list (got)
                 (if (listp got)
                     (mapcar (lambda (elem)
                               (if (atom elem) elem (car elem)))
                             got)
                     (list got))))
        (mapcan #'make-a-list
                (list required optional rest
                      (mapcan (lambda (key) (list (make-keyword key) key)) keys))))))

  (defun make-table-key (sym)
    (format nil "~A::~A" (package-name (symbol-package sym)) (symbol-name sym)))

  (defun convert-specialized-lambda-list-to-lambda-list (specialized-lambda-list)
    "Ex. (a (b type-b) &key (c 10)) -> (a b &key (c 10))"
    (let* ((count-required (aif (position-if (lambda (elem)
                                               (and (atom elem)
                                                    (string= (subseq (symbol-name elem) 0 1)
                                                             "&")))
                                             specialized-lambda-list)
                                it
                                (length specialized-lambda-list)))
           (required (subseq specialized-lambda-list 0 count-required))
           (rest (nthcdr count-required specialized-lambda-list)))
      (append (mapcar (lambda (elem) (if (atom elem) elem (car elem)))
                      required)
              rest)))

  ;; - misc - ;;
  (defun parse-defgeneric-options (options)
    (let ((result (make-defgeneric-options)))
      (dolist (opt options)
        (let ((kind (car opt))
              (rest (rest opt)))
          (ecase kind
            (:documentation
             (let ((doc (car rest)))
               (check-type doc string)
               (setf (defgeneric-options-documentation result) doc))))))
      result)))

(defun.ps instance-dispatch-p (test-instance target-type)
  (or (null target-type)
      (instanceof test-instance target-type)))

(defun.ps+ instance-list-dispatch-p (test-instance-list target-type-list)
  "Note: Used for dispatching in execution time"
  (assert (= (length test-instance-list) (length target-type-list)))
  (loop
     for test-instance in test-instance-list
     for target-type in target-type-list
     do (unless (instance-dispatch-p test-instance target-type)
          (return-from instance-list-dispatch-p nil)))
  t)

(defun.ps type-dispatch-p (test-type target-type)
  (or (and (null test-type)
           (null target-type))
      (and (not (null test-type))
           (instance-dispatch-p (new (test-type)) target-type))))

(defun.ps+ type-list-dispatch-p (test-type-list target-type-list)
  "Note: Used for definition"
  (assert (= (length test-type-list) (length target-type-list)))
  (loop
     for test-type in test-type-list
     for target-type in target-type-list
     do (unless (type-dispatch-p test-type target-type)
          (return-from type-list-dispatch-p nil)))
  t)

(defun.ps+ type-prior-p (test-type target-type)
  (and (not (eq test-type target-type))
       (type-dispatch-p test-type target-type)))

(defun.ps+ compare-dispatch-prior (type-list-a type-list-b)
  (assert (= (length type-list-a) (length type-list-b)))
  (dotimes (i (length type-list-a))
    (let ((type-a (nth i type-list-a))
          (type-b (nth i type-list-b)))
      (cond ((type-prior-p type-a type-b)
             (return-from compare-dispatch-prior -1))
            ((type-prior-p type-b type-a)
             (return-from compare-dispatch-prior 1)))))
  0)

(defun.ps sort-dispatch-item-list (list)
  (list.sort (lambda (a b)
               (compare-dispatch-prior (dispatch-item-type-list a)
                                       (dispatch-item-type-list b)))))

(defun.ps+ find-dispatch-func (function-name instance-list)
  (let ((dispatch-item-list (gethash function-name *dispatch-list-table*)))
    (unless dispatch-item-list
      (error "There is no generic function \"~A\"" function-name))
    (dolist (item dispatch-item-list)
      (when (instance-list-dispatch-p instance-list (dispatch-item-type-list item))
        (return-from find-dispatch-func (dispatch-item-func item)))))
  (error "Can't find a function for ~A" instance-list))

(defun.ps+ same-type-list-p (type-list-a type-list-b)
  (unless (= (length type-list-a) (length type-list-b))
    (return-from same-type-list-p nil))
  (dotimes (i (length type-list-a))
    (unless (eq (nth i type-list-a) (nth i type-list-b))
      (return-from same-type-list-p nil)))
  t)

(defun.ps push-dispatch-func (function-name type-list func)
  (symbol-macrolet ((item-list (gethash function-name *dispatch-list-table*)))
    (setf item-list
          (item-list.filter
           (lambda (item)
             (not (same-type-list-p (dispatch-item-type-list item) type-list)))))
    (push (make-dispatch-item :type-list type-list
                              :func func)
          item-list)
    (sort-dispatch-item-list item-list)))
