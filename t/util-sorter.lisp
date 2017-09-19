(in-package :cl-user)
(defpackage ps-experiment-test.util-sorter
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment.util-sorter
        :prove)
  (:import-from :anaphora
                :aif
                :it))
(in-package :ps-experiment-test.util-sorter)

;; --- preparing --- ;;

(defstruct test-node name children)

(defmethod get-node-name ((node test-node))
  (test-node-name node))

(defmethod node-equalp ((node1 test-node) (node2 test-node))
  (eq (test-node-name node1) (test-node-name node2)))

(defmethod get-children ((node test-node))
  (test-node-children node))

;; ((:parentA :childA1 :childA2) (:parentB :childB1 :childB2) ...)
(defun make-tree (parent-children-pair)
  (let ((node-pool (make-hash-table))
        (result nil))
    (flet ((ensure-node (name)
             (check-type name keyword)
             (aif (gethash name node-pool)
                  it
                  (setf (gethash name node-pool)
                        (make-test-node :name name)))))
      (dolist (pair parent-children-pair)
        (let ((parent (ensure-node (car pair))))
          (dolist (child-name (cdr pair))
            (push (ensure-node child-name)
                  (test-node-children parent)))
          (push parent result))))
    (dolist (node result)
      (setf (test-node-children node)
            (nreverse (test-node-children node))))
    result))

;; - trees - ;;

(defparameter *simple-tree*
  '((:a :b :c) (:c :d :e) (:d :f :g)))

;; The node :f has two parents, :a and :d.
(defparameter *duplicated-tree*
  '((:a :b :f :c) (:c :d :e) (:d :f :g) (:f :h :i)))

;; Note: Self dependency should not raise errors
(defparameter *tree-to-test-self-dependency*
    '((:a :a :b :f :c) (:c :c :d :e) (:d :f :g) (:f :h :i)))

;; The nodes :g, :d and :f have circular dependency.
(defparameter *circular-tree*
  '((:a :b :f :c) (:f :h :g) (:g :d) (:d :f) (:c :d :e)))

;; --- testing --- ;;

(plan 1)

;; TODO: Test utilities should be tested (especially make-tree).

(defun is-sorted (tree expected)
  (is (mapcar #'get-node-name (sort-tree-node (make-tree tree)))
      expected
      :test #'equal))

(defun is-sort-error (tree)
  (is-error (sort-tree-node (make-tree tree))
            'simple-error))

(subtest
    "Test sort-tree-node"
  (is-sorted *simple-tree* '(:B :E :G :F :D :C :A))
  (is-sorted *duplicated-tree* '(:B :E :G :I :H :F :D :C :A))
  (is-sorted *tree-to-test-self-dependency* '(:B :E :G :I :H :F :D :C :A))
  (is-sort-error *circular-tree*))

(finalize)
