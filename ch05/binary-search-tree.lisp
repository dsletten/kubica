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

(defpackage :binary-search-tree (:use :common-lisp :core :test))

(in-package :binary-search-tree)

(defclass tree-node ()
  ((value :accessor value :initarg :value)
   (left :accessor left :initform nil)
   (right :accessor right :initform nil)
   (parent :accessor parent :initform nil :initarg :parent))) ; ROOT get :INITFORM, every other node gets :INITARG

(defclass binary-search-tree ()
  ((root :reader root :initform nil)))

(defun insert-tree-node (tree value)
  (with-slots (root) tree
    (if (null root)
        (setf root (make-instance 'tree-node :value value))
        (insert-node root value))))

(defun insert-node (current new-value &key (test #'<))
  (with-slots (value left right parent) current
    (cond ((funcall test new-value value)
           (if (null left)
               (setf left (make-instance 'tree-node :value new-value :parent current))
               (insert-node left new-value)))
          ((funcall test value new-value)
           (if (null right)
               (setf right (make-instance 'tree-node :value new-value :parent current))
               (insert-node right new-value)))
          (t :node-exists))))

(defun find-tree-node (tree target)
  (with-slots (root) tree
    (if (null root)
        nil
        (find-value root target))))
  
(defun find-value (current target &key (test #'<))
  (if (null current)
      nil
      (with-slots (value left right) current
        (cond ((funcall test target value) (find-value left target))
              ((funcall test value target) (find-value right target))
              (t current)))) )

          
(defun find-successor (node)
  (labels ((traverse-left-branch (node)
             (if (null (left node))
                 node
                 (traverse-left-branch (left node)))) )
    (if (null (right node))
        nil
        (traverse-left-branch (right node)))) )
