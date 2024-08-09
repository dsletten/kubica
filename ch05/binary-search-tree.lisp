;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               binary-search-tree.lisp
;;;;
;;;;   Started:            Fri Jul  5 19:42:29 2024
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :containers (:use :common-lisp :core :test) (:shadow :count :emptyp))

(in-package :containers)

; Root is its own parent???
(defclass tree-node ()
  ((value :accessor value :initarg :value)
   (left :accessor left :initform nil)
   (right :accessor right :initform nil)
   (parent :accessor parent :initform nil :initarg :parent))) ; ROOT gets :INITFORM, every other node gets :INITARG

(defmethod print-object ((node tree-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "(~A)->~A L: ~A R: ~A"
            (if (parent node) (value (parent node)) "∅")
            (value node)
            (if (left node) (value (left node)) "∅")
            (if (right node) (value (right node)) "∅"))))

(defun unlink (node)
  (with-slots (left right parent) node
    (setf left nil
          right nil
          parent nil)))

(defgeneric find-successor (node)
  (:documentation "Find the node (if any) that follows NODE in a tree."))
(defmethod find-successor ((node tree-node))
  (labels ((traverse-left-branch (node)
             (if (null (left node))
                 node
                 (traverse-left-branch (left node))))
           (find-ancestor (node)
             (with-slots (parent) node
               (cond ((null parent) nil) ; Root
                     ((eq node (left parent)) parent)
                     (t (find-ancestor parent)))) ))
    (if (null (right node))
        (find-ancestor node)
        (traverse-left-branch (right node)))) )

;find-predecessor

; Can't include parent in persistent node. Requires every node to be updated on every insertion/deletion!
(defclass persistent-tree-node ()
  ((value :reader value :initarg :value)
   (left :reader left :initform nil :initarg :left)
   (right :reader right :initform nil :initarg :right)))

(defmethod print-object ((node persistent-tree-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A L: ~A R: ~A"
            (value node)
            (if (left node) (value (left node)) "∅")
            (if (right node) (value (right node)) "∅"))))

;;;
;;;    This is incomplete, but it works for the node removal algorithm!!
;;;    Node with 2 children always has a right branch.
;;;    
(defmethod find-successor ((node persistent-tree-node))
  (labels ((traverse-left-branch (node)
             (if (null (left node))
                 node
                 (traverse-left-branch (left node)))) )
    (traverse-left-branch (right node))))

;;;
;;;    BST may accommodate different notions of order, e.g., <, string<
;;;    But the left child must be "less than" the parent and the parent
;;;    "less than" the right child. Otherwise, functions such as FIND-SUCCESSOR
;;;    will be break.
;;;    
(defclass binary-search-tree ()
  ((test :reader test :initform #'< :initarg :test))
  (:documentation "For every node in the tree, the values of nodes to the left are less than the value of the given node. The values of every node to the right are greater."))

(defmethod emptyp ((tree binary-search-tree))
  (zerop (size tree)))

(defun inorder-traversal (tree)
  (labels ((traverse (node)
             (unless (null (left node))
               (traverse (left node)))
             (print (value node))
             (unless (null (right node))
               (traverse (right node)))) )
    (traverse (root tree))))

(defgeneric find-tree-node (tree target)
  (:documentation "Locate the node in TREE with the value TARGET."))
(defmethod find-tree-node ((tree binary-search-tree) target)
  (with-slots (root test) tree
    (labels ((find-node (current)
               (if (null current)
                   nil
                   (with-slots (value left right) current
                     (cond ((funcall test target value) (find-node left))
                           ((funcall test value target) (find-node right))
                           (t current)))) ))
    (if (null root)
        nil
        (find-node root)))) )

(defclass mutable-binary-search-tree (binary-search-tree)
  ((root :reader root :initform nil)
   (count :reader count :initform 0)))

(defmethod size ((tree mutable-binary-search-tree))
  (count tree))

(defgeneric insert-tree-node (tree new-value)
  (:documentation "Insert new node into TREE with the given NEW-VALUE."))
(defmethod insert-tree-node ((tree mutable-binary-search-tree) new-value)
  (with-slots (root count test) tree
    (labels ((insert-node (current)
               (with-slots (value left right parent) current
                 (cond ((funcall test new-value value)
                        (if (null left)
                            (setf left (make-instance 'tree-node :value new-value :parent current))
                            (insert-node left)))
                       ((funcall test value new-value)
                        (if (null right)
                            (setf right (make-instance 'tree-node :value new-value :parent current))
                            (insert-node right)))
                       (t (error "Node with value: ~A already exists." new-value)))) ))
      (if (null root)
          (setf root (make-instance 'tree-node :value new-value))
          (insert-node root))
      (incf count))))

(defgeneric remove-tree-node (tree node)
  (:documentation "Remove NODE (if present) from TREE."))
;;;
;;;    Kludge for decreasing COUNT!
;;;    REMOVE-DOUBLE-CHILD-NODE decreases when it removes the successor.
;;;    The recursive call will be to REMOVE-LEAF-NODE/REMOVE-SINGLE-CHILD-NODE.
;;;    Don't need separate DECF.
;;;    
;; (defmethod remove-tree-node ((tree mutable-binary-search-tree) (node tree-node))
;;   (with-slots (root count) tree
;;     (labels ((fail (target)
;;                (error "Target was not found in this tree: ~A" target))
;;              (remove-leaf-node ()
;;                (if (null (parent node))
;;                    (setf root nil)
;;                    (with-slots (left right parent) node
;;                      (assert (and (null left) (null right)) () "Node is not a leaf.")
;;                      (update-child-link parent node nil)
;;                      (setf parent nil)))
;;                (decf count))
;;              (remove-single-child-node (child)
;;                (with-slots (parent) node
;;                  (setf (parent child) parent)
;;                  (if (null parent)
;;                      (setf root child)
;;                      (update-child-link parent node child)))
;;                (unlink node)
;;                (decf count))
;;              (remove-double-child-node (successor)
;;                (remove-tree-node tree successor)
;;                (with-slots (left right parent) node
;;                  (if (null parent)
;;                      (setf root successor)
;;                      (update-child-link parent node successor))
;;                  (setf (parent successor) parent
;;                        (left successor) left
;;                        (parent left) successor
;;                        (right successor) right)
;;                  (unless (null right)
;;                    (setf (parent right) successor)))
;;                (unlink node))
;;              (update-child-link (parent node child)
;;                (if (eq node (left parent))
;;                    (setf (left parent) child)
;;                    (setf (right parent) child))))
;;     (if (null root)
;;         (fail node)
;;         (with-slots (left right) node
;;           (cond ((and (null left) (null right)) (remove-leaf-node))
;;                 ((or (null left) (null right)) (remove-single-child-node (or left right)))
;;                 (t (remove-double-child-node (find-successor node)))) )))) )

(defmethod remove-tree-node ((tree mutable-binary-search-tree) value)
  (with-slots (root count) tree
    (labels ((fail (target)
               (error "Target was not found in this tree: ~A" target))
             (replace-node (node replacement)
               (unless (null replacement)
                 (setf (parent replacement) (parent node)))
               (if (eq node root)
                   (setf root replacement)
                   (update-child-link (parent node) node replacement)))
             (remove-node (node replacement)
               (replace-node node replacement)
               (unlink node)
               (decf count))
             (remove-double-child-node (node successor)
               (remove-tree-node tree successor)
               (replace-node node successor)
               (with-slots (left right) node
                 (setf (left successor) left
                       (parent left) successor
                       (right successor) right)
                 (unless (null right)
                   (setf (parent right) successor)))
               (unlink node))
             (update-child-link (parent node child)
               (if (eq node (left parent))
                   (setf (left parent) child)
                   (setf (right parent) child))))
    (if (null root)
        (fail value)
        (let ((node (find-tree-node tree value)))
          (if (null node)
              (fail value)
              (with-slots (left right) node
                (cond ((and (null left) (null right)) (remove-node node nil))
                      ((or (null left) (null right)) (remove-node node (or left right)))
                      (t (remove-double-child-node node (find-successor node)))) )))) )))

;;;
;;;    Persistent BST may accommodate different notions of order, e.g., <, string<
;;;    But the left child must be "less than" the parent and the parent
;;;    "less than" the right child. Otherwise, functions such as FIND-SUCCESSOR
;;;    will be break.
;;;    
(defclass persistent-binary-search-tree (binary-search-tree)
  ((root :reader root :initform nil :initarg :root)
   (count :reader count :initform 0 :initarg :count)))

(defmethod size ((tree persistent-binary-search-tree))
  (count tree))

(defun build-persistent-binary-search-tree (values &optional (test #'<))
  (loop for value in values
        for tree = (insert-tree-node (make-instance 'persistent-binary-search-tree :test test) value) then (insert-tree-node tree value)
        finally (return tree)))

;;;
;;;    Only one should be non-NIL?
;;;    
(defun update-persistent-tree-node (node &key (left nil leftp) (right nil rightp))
  (make-instance 'persistent-tree-node
                 :value (value node)
                 :left (if leftp left (left node))
                 :right (if rightp right (right node))))

(defmethod insert-tree-node ((tree persistent-binary-search-tree) new-value)
  (with-slots (root count test) tree
    (labels ((insert-node (current)
               (with-slots (value left right) current
                 (cond ((funcall test new-value value)
                        (update-persistent-tree-node current
                                                     :left (if (null left)
                                                               (make-instance 'persistent-tree-node :value new-value)
                                                               (insert-node left))))
                       ((funcall test value new-value)
                        (update-persistent-tree-node current
                                                     :left left
                                                     :right (if (null right)
                                                                (make-instance 'persistent-tree-node :value new-value)
                                                                (insert-node right))))
                       (t (error "Node with value: ~A already exists." new-value)))) ))
      (make-instance 'persistent-binary-search-tree
                     :root (if (null root)
                               (make-instance 'persistent-tree-node :value new-value)
                               (insert-node root))
                     :count (1+ count)
                     :test test))))

;; (defmethod remove-tree-node ((tree persistent-binary-search-tree) (node persistent-tree-node))
;;   (with-slots (root count test) tree
;;     (labels ((fail (target)
;;                (error "Target was not found in this tree: ~A" target))
;;              (remove-node (node target child)
;;                (cond ((null node) (fail target))
;;                      ((funcall test (value target) (value node))
;;                       (update-persistent-tree-node node :left (remove-node (left node) target child)))
;;                      ((funcall test (value node) (value target))
;;                       (update-persistent-tree-node node :right (remove-node (right node) target child)))
;;                      (t child)))
;;              (remove-double-child-node (node target successor)
;;                (cond ((null node) (fail target))
;;                      ((funcall test (value target) (value node))
;;                       (update-persistent-tree-node node :left (remove-double-child-node (left node) target successor)))
;;                      ((funcall test (value node) (value target))
;;                       (update-persistent-tree-node node :right (remove-double-child-node (right node) target successor)))
;;                      (t (update-persistent-tree-node successor :left (left node) :right (right node)))) ))
;;     (if (null root)
;;         (fail node)
;;         (with-slots (left right) node
;;           (let ((new-root (cond ((and (null left) (null right)) (remove-node root node nil))
;;                                 ((or (null left) (null right)) (remove-node root node (or left right)))
;;                                 (t (let* ((successor (find-successor node))
;;                                           (new-tree (remove-tree-node tree successor)))
;;                                      (remove-double-child-node (root new-tree) node successor)))) ))
;;             (make-instance 'persistent-binary-search-tree :root new-root :count (1- count) :test test)))) )))

;;;
;;;    Persistent tree has to rebuild starting from the root down.
;;;    May as well locate the node to delete (by its value) along the way:
;;;    1. No separate search
;;;    2. Verifies that the node is actually in the tree!
;;;    
(defmethod remove-tree-node ((tree persistent-binary-search-tree) value)
  (with-slots (root count test) tree
    (labels ((fail (target)
               (error "Target was not found in this tree: ~A" target))
             (remove-node (current value)
               (cond ((null current) (fail value))
                     ((funcall test value (value current))
                      (update-persistent-tree-node current :left (remove-node (left current) value)))
                     ((funcall test (value current) value)
                      (update-persistent-tree-node current :right (remove-node (right current) value)))
                     (t (with-slots (left right) current
                          (cond ((and (null left) (null right)) nil)
                                ((or (null left) (null right)) (or left right))
                                (t (let* ((successor (find-successor current))
                                          (new-node (remove-node current (value successor))))
                                     (update-persistent-tree-node successor :left (left new-node) :right (right new-node)))) )))) ))
    (if (null root)
        (fail value)
        (make-instance 'persistent-binary-search-tree :root (remove-node root value) :count (1- count) :test test)))) )

