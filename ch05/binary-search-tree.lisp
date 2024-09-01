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

(defpackage :containers (:use :common-lisp :core :test) (:shadow :count :emptyp :find :remove))

(in-package :containers)

; Root is its own parent???
(defclass tree-node ()
  ((value :reader value :initarg :value)
   (left :accessor left :initform nil)
   (right :accessor right :initform nil)
   (parent :accessor parent :initform nil :initarg :parent))) ; ROOT gets :INITFORM, every other node gets :INITARG

(defmethod print-object ((node tree-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "(~A) ~A ↙ ~A ↘ ~A"
            (if (parent node) (value (parent node)) "∅")
            (if (left node) (value (left node)) "∅")
            (value node)
            (if (right node) (value (right node)) "∅"))))

(defun unlink (node)
  (setf (left node) nil
        (right node) nil
        (parent node) nil))

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
    (format stream "~A ↙ ~A ↘ ~A"
            (if (left node) (value (left node)) "∅")
            (value node)
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
;;;    Only one should be non-NIL?
;;;    
(defun update-persistent-tree-node (node &key (left nil leftp) (right nil rightp))
  (make-instance 'persistent-tree-node
                 :value (value node)
                 :left (if leftp left (left node))
                 :right (if rightp right (right node))))

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

(defgeneric less-than-p (binary-search-tree value1 value2))
(defmethod less-than-p ((bst binary-search-tree) value1 value2)
  (funcall (test bst) value1 value2))

(defgeneric greater-than-p (binary-search-tree value1 value2))
(defmethod greater-than-p ((bst binary-search-tree) value1 value2)
  (funcall (test bst) value2 value1))

(defun inorder-traversal (tree)
  (labels ((traverse (node)
             (unless (null (left node))
               (traverse (left node)))
             (print (value node))
             (unless (null (right node))
               (traverse (right node)))) )
    (traverse (root tree))))

(defgeneric find (tree target)
  (:documentation "Locate the node in TREE with the value TARGET."))
(defmethod find ((tree binary-search-tree) target)
  (labels ((find-node (current)
             (cond ((null current) nil)
                   ((less-than-p tree target (value current)) (find-node (left current)))
                   ((greater-than-p tree target (value current)) (find-node (right current)))
                   (t current))))
    (find-node (root tree))))

(defclass mutable-binary-search-tree (binary-search-tree)
  ((root :accessor root :initform nil)
   (count :accessor count :initform 0)))

(defmethod size ((tree mutable-binary-search-tree))
  (count tree))

(defgeneric insert (tree new-value)
  (:documentation "Insert new node into TREE with the given NEW-VALUE."))
(defmethod insert ((tree mutable-binary-search-tree) new-value)
  (labels ((insert-node (current)
             (with-slots (value left right) current
               (cond ((less-than-p tree new-value value)
                      (if (null left)
                          (setf left (make-instance 'tree-node :value new-value :parent current))
                          (insert-node left)))
                     ((greater-than-p tree new-value value)
                      (if (null right)
                          (setf right (make-instance 'tree-node :value new-value :parent current))
                          (insert-node right)))
                     (t (error "Node with value: ~A already exists." new-value)))) ))
    (if (null (root tree))
        (setf (root tree) (make-instance 'tree-node :value new-value))
        (insert-node (root tree)))) )
(defmethod insert :after ((tree mutable-binary-search-tree) new-value)
  (declare (ignore new-value))
  (incf (count tree)))

(defgeneric remove (tree value)
  (:documentation "Remove node containing VALUE (if present) from TREE."))
(defmethod remove ((tree mutable-binary-search-tree) value)
  (labels ((fail (target)
             (error "Target was not found in this tree: ~A" target))
           (replace-node (node replacement)
             (unless (null replacement)
               (setf (parent replacement) (parent node)))
             (if (eq node (root tree))
                 (setf (root tree) replacement)
                 (update-child-link (parent node) node replacement)))
           (remove-double-child-node (node successor)
             (replace-node successor (right successor))
             (unlink successor) ; Not really necessary. All links will be updated.
             (replace-node node successor)
             (with-slots (left right) node
               (setf (left successor) left
                     (parent left) successor
                     (right successor) right)
               (unless (null right)
                 (setf (parent right) successor))))
           (update-child-link (parent child new-child)
             (cond ((eq child (left parent)) (setf (left parent) new-child))
                   ((eq child (right parent)) (setf (right parent) new-child))
                   (t (error "No parent-child relationship.")))) )
    (let ((node (find tree value)))
      (if (null node)
          (fail value)
          (with-slots (left right) node
            (if (or (null left) (null right))
                (replace-node node (or right left))
                (remove-double-child-node node (find-successor node)))
            (unlink node)))) ))
(defmethod remove :after ((tree mutable-binary-search-tree) value)
  (declare (ignore value))
  (decf (count tree)))

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

(defmethod insert ((tree persistent-binary-search-tree) new-value)
  (labels ((insert-node (current)
             (with-slots (value left right) current
               (cond ((less-than-p tree new-value value)
                      (update-persistent-tree-node current
                                                   :left (if (null left)
                                                             (make-instance 'persistent-tree-node :value new-value)
                                                             (insert-node left))))
                     ((greater-than-p tree new-value value)
                      (update-persistent-tree-node current
                                                   :right (if (null right)
                                                              (make-instance 'persistent-tree-node :value new-value)
                                                              (insert-node right))))
                     (t (error "Node with value: ~A already exists." new-value)))) ))
    (make-instance 'persistent-binary-search-tree
                   :root (if (null (root tree))
                             (make-instance 'persistent-tree-node :value new-value)
                             (insert-node (root tree)))
                   :count (1+ (count tree))
                   :test (test tree))))

;;;
;;;    Persistent tree has to rebuild starting from the root down.
;;;    May as well locate the node to delete (by its value) along the way:
;;;    1. No separate search
;;;    2. Verifies that the node is actually in the tree!
;;;    
(defmethod remove ((tree persistent-binary-search-tree) value)
  (labels ((fail (target)
             (error "Target was not found in this tree: ~A" target))
           (remove-node (current value)
             (cond ((null current) (fail value))
                   ((less-than-p tree value (value current))
                    (update-persistent-tree-node current :left (remove-node (left current) value)))
                   ((greater-than-p tree value (value current))
                    (update-persistent-tree-node current :right (remove-node (right current) value)))
                   (t (with-slots (left right) current
                        (if (or (null left) (null right))
                            (or right left)
                            (let* ((successor (find-successor current))
                                   (new-node (remove-node current (value successor))))
                              (update-persistent-tree-node successor :left (left new-node) :right (right new-node)))) )))) )
    (if (null (root tree))
        (fail value)
        (make-instance 'persistent-binary-search-tree :root (remove-node (root tree) value) :count (1- (count tree)) :test (test tree)))) )

