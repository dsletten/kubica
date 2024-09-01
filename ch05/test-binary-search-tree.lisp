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

(defvar *kubica-vals* '(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95))

(defun kubica-initialize-tree (bst)
  (dolist (x '(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95))
    (insert bst x)))

(defun build-persistent-binary-search-tree (values &optional (test #'<))
  (loop for value in values
        for tree = (insert (make-instance 'persistent-binary-search-tree :test test) value) then (insert tree value)
        finally (return tree)))

(defun locate-unique-node (bst generator)
  (let ((value (funcall generator)))
    (if (find bst value)
        (locate-unique-node bst generator)
        value)))

(defun initialize-string-tree (bst)
  (let ((words #("angel" "angeline" "bar" "baz" "clean" "come" "danger" "devil" "duration"
                 "foo" "harm" "hide" "knock" "losing" "mister" "pitchfork" "pretty" "pung"
                 "show" "stranger" "there" "vacation"))
        (indexes #(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95))
        (sorted-indexes (sort (vector 50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95) #'<)))
    (dotimes (i (length words))
      (format t "~A ~D ~D ~D~%" (aref words (position (aref indexes i) sorted-indexes))
              i (aref indexes i) (position (aref indexes i) sorted-indexes))
      (insert bst (aref words (position (aref indexes i) sorted-indexes)))) ))

(deftest test-size (&optional (n 100))
  (check
   (let ((bst (make-instance 'mutable-binary-search-tree)))
     (loop for i from 1 to n
           for value = (locate-unique-node bst #'(lambda () (random 1000)))
           do (insert bst value)
              (assert (= i (size bst)) () "Tree should have ~D node~:P~%" i))
     (loop for i from (1- n) downto 0
           for value = (value (root bst))
           do (remove bst value)
              (assert (= i (size bst)) () "Tree should have ~D node~:P~%" i))
     t)))

;; (deftest test-persistent-size (&optional (n 100))
;;   (check
;;    (loop for i from 0 to n
;;          for value = (locate-unique-node bst #'(lambda () (random 1000)))
;;          for bst = (make-instance 'persistent-binary-search-tree) then (insert bst value)
;;          do (assert (= i (size bst)) () "Tree should have ~D node~:P~%" i)
;;          finally (return (loop for i from n downto 0
;;                                for value = (value (root bst)) then (value (root bst1))
;;                                for bst1 = bst then (remove bst1 value)
;;                                do (assert (= i (size bst1)) () "Tree should have ~D node~:P~%" i)
;;                                finally (return t)))) ))

(deftest test-persistent-size (&optional (n 100))
  (labels ((grow (i bst)
             (if (= i n)
                 (shrink n bst)
                 (let ((value (locate-unique-node bst #'(lambda () (random 1000)))) )
                   (assert (= i (size bst)) () "Tree should have ~D node~:P~%" i)
                   (grow (1+ i) (insert bst value)))) )
           (shrink (i bst)
             (if (zerop i)
                 t
                 (let ((value (value (root bst))))
                   (assert (= i (size bst)) () "Tree should have ~D node~:P~%" i)
                   (shrink (1- i) (remove bst value)))) ))
  (check
   (grow 0 (make-instance 'persistent-binary-search-tree)))))

(deftest test-insert ()
  (check
   (let ((bst (make-instance 'mutable-binary-search-tree)))
     (dotimes (i 400 t)
       (let ((value (locate-unique-node bst #'(lambda () (random 1000)))) )
         (insert bst value)
         (assert (= value (value (find bst value))) () "Tree does not contain ~D" value)))) ))

(deftest test-persistent-insert ()
  (check
   (let ((bst (make-instance 'persistent-binary-search-tree)))
     (dotimes (i 400 t)
       (let ((value (locate-unique-node bst #'(lambda () (random 1000)))) )
         (setf bst (insert bst value))
         (assert (= value (value (find bst value))) () "Tree does not contain ~D" value)))) ))

(deftest test-find ()
  (check
   (let ((bst (make-instance 'mutable-binary-search-tree)))
     (kubica-initialize-tree bst)
     (dolist (val *kubica-vals* t)
       (assert (= val (value (find bst val))) () "Tree does not contain ~D" val)))) )

(deftest test-persistent-find ()
  (check
   (let ((bst (build-persistent-binary-search-tree *kubica-vals*)))
     (dolist (val *kubica-vals* t)
       (assert (= val (value (find bst val))) () "Tree does not contain ~D" val)))) )

(deftest test-find-successor ()
  (check
   (let ((bst (make-instance 'mutable-binary-search-tree))
         (indexes '(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95))
         (sorted-indexes (sort (list 50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95) #'<)))
     (dolist (x indexes)
       (insert bst x))
     (loop for elt in sorted-indexes
           for next in (rest sorted-indexes)
           for successor = (value (find-successor (find bst elt)))
           do (assert (= next successor) () "Successor to ~D should be ~D. Found: ~D" elt next successor))
     t)
   (let ((bst (make-instance 'mutable-binary-search-tree :test #'string<))
         (indexes (mapcar (partial #'format nil "~D") '(50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95)))
         (sorted-indexes (sort (mapcar (partial #'format nil "~D") (list 50 23 67 14 38 60 81 6 17 27 42 59 63 78 92 1 7 21 29 58 91 95)) #'string<)))
     (dolist (x indexes)
       (insert bst x))
     (loop for elt in sorted-indexes
           for next in (rest sorted-indexes)
           for successor = (value (find-successor (find bst elt)))
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






