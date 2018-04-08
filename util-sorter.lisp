(in-package :cl-user)
(defpackage ps-experiment/util-sorter
  (:use :cl
        :parenscript)
  (:export :get-node-name
           :node-equalp
           :get-children
           :sort-tree-node)
  (:import-from :anaphora
                :awhen
                :aif
                :it))
(in-package :ps-experiment/util-sorter)

;; This sorts nodes in a tree considering its dependency.
;; It will be used for solving package dependency.

;; Note: A simple depth-first-search (DFS) is not enough when a node has multiple parent nodes.
;; In such case, the node can be visited before a parent is visited by DFS.


;; --- generics --- ;;

(defgeneric get-node-name (node))
(defgeneric node-equalp (node1 node2))
(defgeneric get-children (node))

;; --- node functinos --- ;;

(defun all-children-are-processed (node processed-node-list)
  (every (lambda (child) (or (node-equalp node child) ; Ignore self dependency
                             (find child processed-node-list :test #'node-equalp)))
         (get-children node)))

(defun linearize-all-nodes (node-list)
  "Extract all nodes without duplication and linearlize them into a list."
  (let ((result nil))
    (labels ((rec (node)
               (unless (some (lambda (target) (node-equalp target node))
                             result)
                 (push node result)
                 (dolist (child (get-children node))
                   (rec child)))))
      (dolist (node node-list)
        (rec node)))
    result))

(defun extract-circular-nodes (node-list)
  "Extract a node list that includes circular dependency. But self-dependency is ignored."
  (labels ((rec (current-node traverse-list)
             (setf traverse-list (cons current-node traverse-list))
             (dolist (child (get-children current-node))
               (unless (node-equalp current-node child) ; Ignore self dependency
                 (when (find child traverse-list :test #'node-equalp)
                   (let ((result (member child (reverse traverse-list)
                                         :test #'node-equalp)))
                     (return-from rec result)))
                 (let ((next-result (rec child traverse-list)))
                   (when next-result
                     (return-from rec next-result)))))
             nil))
    (dolist (node node-list)
      (awhen (rec node nil)
        (return-from extract-circular-nodes it)))))

(defun check-circular-dependency (node-list)
  "Check if the node-list has a circular dependency. But self-dependency is ignored."
  (awhen (extract-circular-nodes node-list)
    (error "There is (a) circular dependency: ~A"
           (mapcar #'get-node-name it))))

;; --- sorter --- ;;

(defun sort-tree-node (node-list)
  "Sort nodes in tree considering their parent-children relationship.
A parent will be sorted after all of its children."
  (labels ((rec (rest-nodes result)
             (aif (find-if (lambda (node)
                             (all-children-are-processed node result))
                           rest-nodes)
                  (rec (remove it rest-nodes :test #'node-equalp)
                       (cons it result))
                  result)))
    (let ((all-node (linearize-all-nodes node-list)))
      (check-circular-dependency all-node)
      (reverse (rec all-node nil)))))
