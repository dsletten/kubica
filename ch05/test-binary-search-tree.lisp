;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               test-binary-search-tree.lisp
;;;;
;;;;   Started:            Tue Jul  9 20:03:28 2024
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
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :containers)

(use-package :test)

(defun kubica-initialize-tree (bst)
  (dolist (x '(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95))
    (insert-tree-node bst x)))

(defun insert-unique-node (bst generator)
  (let ((value (funcall generator)))
    (if (find-tree-node bst value)
        (insert-unique-node bst generator)
        (values (insert-tree-node bst value) value))))

(defun initialize-string-tree (bst)
  (let ((words #("angel" "angeline" "bar" "baz" "clean" "come" "danger" "devil" "duration"
                 "foo" "harm" "hide" "knock" "losing" "mister" "pitchfork" "pretty" "pung"
                 "show" "stranger" "there" "vacation"))
        (indexes #(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95))
        (sorted-indexes (sort (vector 50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95) #'<)))
    (dotimes (i (length words))
      (format t "~A ~D ~D ~D~%" (aref words (position (aref indexes i) sorted-indexes))
              i (aref indexes i) (position (aref indexes i) sorted-indexes))
      (insert-tree-node bst (aref words (position (aref indexes i) sorted-indexes)))) ))

(deftest test-size ()
  (check
   (let ((bst (make-instance 'mutable-binary-search-tree)))
     (loop for i from 1 to 100
           do (insert-unique-node bst #'(lambda () (random 1000)))
              (assert (= i (size bst)) () "Tree should have ~D node~:P~%" i))
     (loop for i from 99 downto 0
           do (remove-tree-node bst (value (root bst)))
              (assert (= i (size bst)) () "Tree should have ~D node~:P~%" i))
     t)))

(deftest test-find-successor ()
  (check
   (let ((bst (make-instance 'mutable-binary-search-tree))
         (indexes '(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95))
         (sorted-indexes (sort (list 50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95) #'<)))
     (dolist (x indexes)
       (insert-tree-node bst x))
     (loop for elt in sorted-indexes
           for next in (rest sorted-indexes)
           for successor = (value (find-successor (find-tree-node bst elt)))
           do (assert (= next successor) () "Successor to ~D should be ~D. Found: ~D" elt next successor))
     t)
   (let ((bst (make-instance 'mutable-binary-search-tree :test #'string<))
         (indexes (mapcar (partial #'format nil "~D") '(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95)))
         (sorted-indexes (sort (mapcar (partial #'format nil "~D") (list 50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95)) #'string<)))
     (dolist (x indexes)
       (insert-tree-node bst x))
     (loop for elt in sorted-indexes
           for next in (rest sorted-indexes)
           for successor = (value (find-successor (find-tree-node bst elt)))
           do (assert (string= next successor) () "Successor to ~D should be ~D. Found: ~D" elt next successor))
     t)))

;; (deftest test-update-child-link ()
;;   (check
;;    (let ((bst (make-instance 'mutable-binary-search-tree)))
;;      (dolist (x '(1 2 3))
;;        (insert-tree-node bst x))
;;      (update-child-link (root bst) (find-tree-node bst 2) (find-tree-node bst 3))
;;      (eq (find-tree-node bst 3) (right (root bst))))
;;    (let ((bst (make-instance 'mutable-binary-search-tree)))
;;      (dolist (x '(3 2 1))
;;        (insert-tree-node bst x))
;;      (update-child-link (root bst) (find-tree-node bst 2) (find-tree-node bst 1))
;;      (eq (find-tree-node bst 1) (left (root bst)))) ))






