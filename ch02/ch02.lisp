;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Wed Jun  5 23:18:36 2024
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

(defpackage :ch02 (:use :common-lisp :core :test) (:shadow :search))

(in-package :ch02)

(defun binary-search (a target)
  (let ((low 0)
        (high (1- (length a))))
    (while (<= low high)
      (let* ((mid (truncate (+ low high) 2))
             (current (aref a mid)))
        (cond ((= current target) (return-from binary-search mid))
              ((< current target) (setf low (1+ mid)))
              (t (setf high (1- mid)))) ))
    -1))

(defun binary-search (a target)
  (labels ((search (low high)
             (if (< high low)
                 -1
                 (let* ((mid (truncate (+ low high) 2))
                        (current (aref a mid)))
                   (cond ((= current target) mid)
                         ((< current target) (search (1+ mid) high))
                         (t (search low (1- mid)))) ))))
    (search 0 (1- (length a)))) )

(deftest test-binary-search ()
  (let ((a #(-5 -1 0 3 9 11 15 17 30 35 51 54)))
    (check
     (= 0 (binary-search a -5))
     (= 3 (binary-search a 3))
     (= 4 (binary-search a 9))
     (= 8 (binary-search a 30))
     (= 11 (binary-search a 54))
     (= -1 (binary-search a -8))
     (= -1 (binary-search a 12))
     (= -1 (binary-search a 60)))) )

          
(defpackage :ch02-higher-order (:use :common-lisp :core :test) (:shadow :search))

(in-package :ch02-higher-order)

;; (defun binary-search (a target &optional (test #'<))
;;   (let ((low 0)
;;         (high (1- (length a))))
;;     (loop (if (< high low)
;;               (return -1)
;;               (let* ((mid (truncate (+ low high) 2))
;;                      (current (aref a mid)))
;;                 (cond ((funcall test current target) (setf low (1+ mid)))
;;                       ((funcall test target current) (setf high (1- mid)))
;;                       (t (return mid)))) ))))

;; (defun binary-search (a target &optional (test #'<))
;;   (labels ((search (low high)
;;              (if (< high low)
;;                  -1
;;                  (let* ((mid (truncate (+ low high) 2))
;;                         (current (aref a mid)))
;;                    (cond ((funcall test current target) (search (1+ mid) high))
;;                          ((funcall test target current) (search low (1- mid)))
;;                          (t mid)))) ))
;;     (search 0 (1- (length a)))) )

(defun binary-search (a target &key (key #'identity) (test #'<))
  (labels ((search (low high)
             (if (< high low)
                 -1
                 (let* ((mid (truncate (+ low high) 2))
                        (current (funcall key (aref a mid))))
                   (cond ((funcall test current target) (search (1+ mid) high))
                         ((funcall test target current) (search low (1- mid)))
                         (t mid)))) ))
    (search 0 (1- (length a)))) )

(deftest test-binary-search ()
  (let ((a #(-5 -1 0 3 9 11 15 17 30 35 51 54)))
    (check
     (= 0 (binary-search a -5))
     (= 3 (binary-search a 3))
     (= 4 (binary-search a 9))
     (= 8 (binary-search a 30))
     (= 11 (binary-search a 54))
     (= -1 (binary-search a -8))
     (= -1 (binary-search a 12))
     (= -1 (binary-search a 60))))
  (let ((a (reverse #(-5 -1 0 3 9 11 15 17 30 35 51 54))))
    (check
     (= 0 (binary-search a 54 :test #'>))
     (= 3 (binary-search a 30 :test #'>))
     (= 4 (binary-search a 17 :test #'>))
     (= 8 (binary-search a 3 :test #'>))
     (= 11 (binary-search a -5 :test #'>))
     (= -1 (binary-search a -8 :test #'>))
     (= -1 (binary-search a 12 :test #'>))
     (= -1 (binary-search a 60 :test #'>))))
  (let ((a #("Clojure" "java" "JavaScript" "LISP" "Prolog" "ruby")))
    (check
     (= 0 (binary-search a "clojure" :test #'string-lessp))
     (= 1 (binary-search a "Java" :test #'string-lessp))
     (= -1 (binary-search a "java" :test #'string<)) ; !!!
     (= 2 (binary-search a "JAVASCRIPT" :test #'string-lessp))
     (= 3 (binary-search a "Lisp" :test #'string-lessp))
     (= 4 (binary-search a "prolog" :test #'string-lessp))
     (= 5 (binary-search a "Ruby" :test #'string-lessp))
     (= -1 (binary-search a "C#" :test #'string-lessp))))
  (check ; Duplicate elements
   (= 2 (binary-search #(1 2 3 3d0 4) 3))
   (= 2 (binary-search #(1 2 3 3d0) 3))
   (= 1 (binary-search #(2 3 3d0 4) 3))
   (= 1 (binary-search #(3 3d0 4) 3))
   (= 4 (binary-search #(0 1 2 3 3d0 4) 3)))
  (let ((a #((a . 1) (b . 3) (b . 2) (c . 1) (c . 5))))
    (flet ((symbol< (a b)
             (string< (symbol-name a) (symbol-name b))))
      (check
       (= 2 (binary-search a 'b :test #'symbol< :key #'car))
       (= 3 (binary-search a 'c :test #'symbol< :key #'car))
       (= 0 (binary-search a 'a :test #'symbol< :key #'car)))) ))
;;;
;;;    These cases are meaningless! The array is not sorted with respect to #'string<
;;;    Binary search depends on a sorted array!
;;;    
     ;; (= 0 (binary-search a "Clojure" :test #'string<))
     ;; (= -1 (binary-search a "clojure" :test #'string<)))) )

          
