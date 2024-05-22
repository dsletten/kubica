;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch01.lisp
;;;;
;;;;   Started:            Fri May 10 03:47:38 2024
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

(defpackage :ch01 (:use :common-lisp :core :test) (:shadow :sort))

(in-package :ch01)

;;;
;;;    These versions using ROTATEF are due to a misreading of the pseudocode.
;;;    You don't need to swap pairs of elements--just move the current index up.
;;;    The `current` value preserves the final value to drop into the newly created
;;;    hole.
;;;
;;;    The ROTATEF implementations are correct, but they are doing a lot more assignments
;;;    than necessary. This is particularly true of langs lacking ROTATEF, but the
;;;    macroexpansion has to do the same work.
;;;    
;; (defun insertion-sort (a)
;;   (do ((n (length a))
;;        (i 1 (1+ i)))
;;       ((>= i n) a)
;;     (do ((current (aref a i))
;;          (j (1- i) (1- j)))
;;         ((or (minusp j)
;;              (<= (aref a j) current)))
;;       (rotatef (aref a (1+ j)) (aref a j) current))))

(defun insertion-sort (a)
  (do ((n (length a))
       (i 1 (1+ i)))
      ((>= i n) a)
    (do ((current (aref a i))
         (j (1- i) (1- j)))
        ((or (minusp j)
             (<= (aref a j) current)))
      (rotatef (aref a (1+ j)) (aref a j)))) )

(defun insertion-sort (a)
  (loop for i from 1 below (length a)
        do (loop with current = (aref a i)
                 for j from (1- i) downto 0
                 until (<= (aref a j) current)
                 do (rotatef (aref a j) (aref a (1+ j))))
        finally (return a)))

(defun insertion-sort (a)
  (loop for i from 1 below (length a)
        do (loop for j0 from (1- i) downto 0
                 for j1 from i downto 1
                 until (<= (aref a j0) (aref a j1))
                 do (rotatef (aref a j0) (aref a j1)))
        finally (return a)))

(defun insertion-sort (a)
  (labels ((sort-pass (i)
             (loop for j0 from (1- i) downto 0
                   for j1 from i downto 1
                   until (<= (aref a j0) (aref a j1))
                   do (rotatef (aref a j0) (aref a j1)))) )
    (loop for i from 1 below (length a)
          do (sort-pass i)
          finally (return a))))

;;;
;;;    Correct implementation of pseudocode.
;;;    
(defun insertion-sort (a)
  (do ((n (length a))
       (i 1 (1+ i)))
      ((>= i n) a)
    (do ((current (aref a i))
         (j (1- i) (1- j)))
        ((or (minusp j)
             (<= (aref a j) current))
         (setf (aref a (1+ j)) current)) ; This is not necessary when using ROTATEF above.
      (setf (aref a (1+ j)) (aref a j)))) )

;;;
;;;    This relies on the not so obvious semantics of LOOP...
;;;    The FOR clause in isolation will iterate from i - 1 downto 0...and then
;;;    terminate with j = -1! In this case, set a[j+1] = a[0] to `current`.
;;;    
;;;    This will proceed as long as the other condition is true: a[j] > current.
;;;    If that condition is violated, then a[i+j] has already been shifted (unless
;;;    `current` is already in the right place), so that `current` should be the
;;;    new value of a[i+j].
;;;    
(defun insertion-sort (a)
  (labels ((sort-pass (i)
             (loop with current = (aref a i)
                   for j from (1- i) downto 0
                   while (> (aref a j) current)
                   do (setf (aref a (1+ j)) (aref a j))
                   finally (setf (aref a (1+ j)) current))))
    (loop for i from 1 below (length a)
          do (sort-pass i)
          finally (return a))))

(defun insertion-sort (a)
  (labels ((sort-pass (i)
             (do ((current (aref a i))
                  (j (1- i) (1- j)))
                 ((or (minusp j)
                      (<= (aref a j) current))
                  (setf (aref a (1+ j)) current))
               (setf (aref a (1+ j)) (aref a j)))) )
    (loop for i from 1 below (length a)
          do (sort-pass i)
          finally (return a))))

(defun insertion-sort (a)
  (let ((length (length a)))
    (labels ((sort (i)
               (cond ((>= i length) a)
                     (t (sort-pass (1- i) (aref a i))
                        (sort (1+ i)))) )
             (sort-pass (j current)
               (cond ((or (minusp j) (<= (aref a j) current))
                      (setf (aref a (1+ j)) current))
                     (t (setf (aref a (1+ j)) (aref a j))
                        (sort-pass (1- j) current)))) )
      (sort 1))))

(defun random-ordered-array ()
  (let* ((random-state (make-random-state t))
         (low (random 200 random-state))
         (high (+ low (random 100 random-state))))
    (apply #'vector (loop for i from low upto high collect i))))

(deftest test-insertion-sort ()
  (check
   (equalp #() (insertion-sort #()))
   (equalp #(10) (insertion-sort #(10)))
   (equalp #(10 20) (insertion-sort #(10 20)))
   (equalp #(10 20) (insertion-sort #(20 10)))
   (equalp #(1 1d0 2 3d0 3) (insertion-sort #(3d0 1 2 3 1d0))) ; Stable
   (equalp #(1 2 3 4 5) (insertion-sort #(1 2 3 4 5)))
   (equalp #(1 2 3 4 5) (insertion-sort #(5 4 3 2 1)))
   (equalp #(1/5 1/4 1/3 1/2 1) (insertion-sort #(1 1/3 1/5 1/2 1/4)))
   (let ((a (vector 61 82 67 4 98 20 37 85)))
     (equalp #(4 20 37 61 67 82 85 98) (insertion-sort a)))
   (dotimes (i 100 t)
     (let* ((a (random-ordered-array))
            (b (shuffle (copy-seq a))))
       (unless (equalp a (insertion-sort b))
         (return nil)))) ))

(defpackage :ch01-higher-order (:use :common-lisp :core :test) (:shadow :sort))

(in-package :ch01-higher-order)

;;;
;;;    Nested DO
;;;    
;;;
;;;    This complex TERMINATE test is unnecessary.
;;;    
(defun insertion-sort (a &key (test #'<))
  (flet ((terminate (j current)
           (or (minusp j)
               (funcall test (aref a j) current)
               (not (funcall test current (aref a j)))) ))
    (do ((n (length a))
         (i 1 (1+ i)))
        ((>= i n) a)
      (do ((current (aref a i))
           (j (1- i) (1- j)))
          ((terminate j current)
           (setf (aref a (1+ j)) current))
        (setf (aref a (1+ j)) (aref a j)))) ))

(defun insertion-sort (a &key (test #'<))
  (do ((n (length a))
       (i 1 (1+ i)))
      ((>= i n) a)
    (do ((current (aref a i))
         (j (1- i) (1- j)))
        ((or (minusp j) (not (funcall test current (aref a j)))) ; Simpler
         (setf (aref a (1+ j)) current))
      (setf (aref a (1+ j)) (aref a j)))) )

;;;
;;;    Recursive
;;;    
(defun insertion-sort (a &key (test #'<))
  (let ((length (length a)))
    (labels ((terminate (j current)
               (or (minusp j)
                   (funcall test (aref a j) current)
                   (not (funcall test current (aref a j)))) )
             (sort (i)
               (cond ((>= i length) a)
                     (t (sort-pass (1- i) (aref a i))
                        (sort (1+ i)))) )
             (sort-pass (j current)
               (cond ((terminate j current)
                      (setf (aref a (1+ j)) current))
                     (t (setf (aref a (1+ j)) (aref a j))
                        (sort-pass (1- j) current)))) )
      (sort 1))))

(defun insertion-sort (a &key (test #'<))
  (let ((length (length a)))
    (labels ((sort (i)
               (cond ((>= i length) a)
                     (t (sort-pass (1- i) (aref a i))
                        (sort (1+ i)))) )
             (sort-pass (j current)
               (cond ((or (minusp j) (not (funcall test current (aref a j))))
                      (setf (aref a (1+ j)) current))
                     (t (setf (aref a (1+ j)) (aref a j))
                        (sort-pass (1- j) current)))) )
      (sort 1))))

;;;
;;;    Double LOOP
;;;    
(defun insertion-sort (a &key (test #'<))
  (labels ((sort-pass (i)
             (loop with current = (aref a i)
                   for j from (1- i) downto 0
                   while (funcall test current (aref a j))
                   do (setf (aref a (1+ j)) (aref a j))
                   finally (setf (aref a (1+ j)) current))))
    (loop for i from 1 below (length a)
          do (sort-pass i)
          finally (return a))))

(defun random-ordered-array ()
  (let* ((random-state (make-random-state t))
         (low (random 200 random-state))
         (high (+ low (random 100 random-state))))
    (apply #'vector (loop for i from low upto high collect i))))

(deftest test-insertion-sort ()
  (check
   (equalp #() (insertion-sort (vector)))
   (equalp #(10) (insertion-sort (vector 10)))
   (equalp #(10 20) (insertion-sort (vector 10 20)))
   (equalp #(10 20) (insertion-sort (vector 20 10)))
   (equalp #(1 1d0 2 3d0 3) (insertion-sort (vector 3d0 1 2 3 1d0))) ; Stable
   (equalp #(3d0 3 2 1 1d0) (insertion-sort (vector 3d0 1 2 3 1d0) :test #'>)) ; Stable
   (equalp #(1 2 3 4 5) (insertion-sort (vector 1 2 3 4 5)))
   (equalp #(1 2 3 4 5) (insertion-sort (vector 5 4 3 2 1)))
   (equalp #(5 4 3 2 1) (insertion-sort (vector 1 2 3 4 5) :test #'>))
   (equalp #(5 4 3 2 1) (insertion-sort (vector 5 4 3 2 1) :test #'>))
   (equalp #(1/5 1/4 1/3 1/2 1) (insertion-sort (vector 1 1/3 1/5 1/2 1/4)))
   (equalp #(1 1/2 1/3 1/4 1/5) (insertion-sort (vector 1 1/3 1/5 1/2 1/4) :test #'>))
   (equalp #(1 1/2 1/3 1/4 1/5) (insertion-sort (vector 1 1/3 1/5 1/2 1/4)
                                                :test #'(lambda (m n) (< (denominator m)
                                                                         (denominator n)))) )
   (let ((a (vector 61 82 67 4 98 20 37 85)))
     (equalp #(4 20 37 61 67 82 85 98) (insertion-sort a)))
   (equalp #("pung" "foo" "baz" "bar")
           (insertion-sort (vector "pung" "foo" "bar" "baz") :test #'string>))
   (equalp #("Pung" "FOO" "baz" "BAR")
           (insertion-sort (vector "Pung" "FOO" "BAR" "baz") :test #'string-greaterp))
   (equalp #("bar" "baz" "foo" "pung")
           (insertion-sort (vector "pung" "foo" "bar" "baz") :test #'string<))
   (equalp #("BAR" "baz" "Foo" "pUNG")
           (insertion-sort (vector "pUNG" "Foo" "BAR" "baz") :test #'string-lessp))
   (equalp #("foo" "bar" "baz")
           (insertion-sort (vector "foo" "bar" "baz") :test #'(lambda (a b) (< (length a) (length b)))) ) ; Stable
   (equalp #("foo" "bar" "baz")
           (insertion-sort (vector "foo" "bar" "baz") :test #'(lambda (a b) (> (length a) (length b)))) ) ; Stable
   (equalp #((z . 2) (k . 3) (p . 4) (a . 5) (b . 9))
           (insertion-sort (coerce '((a . 5) (b . 9) (k . 3) (p . 4) (z . 2)) 'vector)
                           :test #'(lambda (a b) (< (cdr a) (cdr b)))) )
   (equalp #((a . 5) (b . 9) (k . 3) (p . 4) (z . 2))
           (insertion-sort (coerce '((B . 9) (A . 5) (K . 3) (P . 4) (Z . 2)) 'vector)
                           :test #'(lambda (a b) (string< (string (car a)) (string (car b)))) ))
   (dotimes (i 100 t)
     (let* ((a (random-ordered-array))
            (b (shuffle (copy-seq a))))
       (unless (equalp a (insertion-sort b))
         (return nil)))) ))
