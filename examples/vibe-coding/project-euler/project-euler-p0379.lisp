;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0379 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0379)

(defun make-mu (limit)
  "Precomputes the Möbius function up to the given limit using a sieve."
  (declare (type fixnum limit))
  (let ((mu (make-array (1+ limit) :element-type 'fixnum :initial-element 1))
        (is-prime (make-array (1+ limit) :element-type 'bit :initial-element 1)))
    (setf (aref is-prime 0) 0
          (aref is-prime 1) 0)
    (iterate (for p from 2 to limit)
      (when (= (aref is-prime p) 1)
        (setf (aref mu p) -1)
        (iterate (for i from (* 2 p) to limit by p)
          (setf (aref is-prime i) 0)
          (setf (aref mu i) (- (aref mu i))))
        (let ((p2 (* p p)))
          (when (<= p2 limit)
            (iterate (for i from p2 to limit by p2)
              (setf (aref mu i) 0))))))
    mu))

(defun iroot3 (x)
  "Calculates the integer cube root of x safely without float inaccuracies."
  (declare (type fixnum x))
  (let ;;((u (floor (expt x #.(/ 1 3d0)))))
      ((u (floor (expt x 1/3))))
    (declare (type fixnum u))
    (iterate 
      (while (> (* u u u) x))
      (decf u))
    (iterate 
      (while (<= (* (1+ u) (1+ u) (1+ u)) x))
      (incf u))
    u))

(defun calc-d3 (x)
  "Calculates D_3(X) = sum_{a*b*c <= x} 1 using symmetry in O(X^{2/3}) time."
  (declare (type fixnum x))
  (let* ((u (iroot3 x))
         (w 0)
         (x1 0)
         (x2 0))
    (declare (type fixnum u w x1 x2))
    
    (iterate (for a from 1 to u)
      (let* ((xa (truncate x a))
             (xa-a (truncate xa a))
             (sq-xa (isqrt xa)))
        (declare (type fixnum xa xa-a sq-xa))
        
        ;; x1 handles cases where a = b < c
        (incf x1 (the fixnum (- xa-a a)))
        ;; x2 handles cases where a < b = c
        (incf x2 (the fixnum (- sq-xa a)))
        
        ;; w handles cases where a < b < c
        ;; This is the innermost loop where the bulk of the computation happens
        (iterate (for b from (1+ a) to sq-xa)
          (incf w (the fixnum (- (truncate xa b) b))))))
          
    ;; Combine combinations with their permutations (3! = 6, 3!/2! = 3)
    (+ (* 6 w) (* 3 x1) (* 3 x2) u)))

(defun solve (&optional (n 1000000000000))
  (declare (type integer n))
  (let* ((limit (isqrt n))
         (mu (make-mu limit))
         (s 0))
    (declare (type fixnum limit)
             (type integer s))
             
    (format t "Calculating g(~D)...~%" n)
    
    (iterate (for c from 1 to limit)
      (let ((mc (aref mu c)))
        (declare (type fixnum mc))
        (unless (= mc 0)
          (let* ((c2 (* c c))
                 (x (truncate n c2))
                 (d3 (calc-d3 x)))
            (declare (type fixnum c2 x d3))
            (incf s (* mc d3)))))
            
      ;; Intermediate logging for progress and debugging
      (when (zerop (mod c 100000))
        (format t "Processed c = ~D / ~D~%" c limit)))
        
    (let ((ans (truncate (+ n s) 2)))
      (format t "Final ans = ~D~%" ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating g(1000000000000)...
Processed c = 100000 / 1000000
Processed c = 200000 / 1000000
Processed c = 300000 / 1000000
Processed c = 400000 / 1000000
Processed c = 500000 / 1000000
Processed c = 600000 / 1000000
Processed c = 700000 / 1000000
Processed c = 800000 / 1000000
Processed c = 900000 / 1000000
Processed c = 1000000 / 1000000
Final ans = 132314136838185

User time    =        7.104
System time  =        0.067
Elapsed time =        7.126
Allocation   = 37468824 bytes
7350 Page faults
GC time      =        0.010
 |------------------------------------------------------------|#
;;→ 132314136838185
:ok