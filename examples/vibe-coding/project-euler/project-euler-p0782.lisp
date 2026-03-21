;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0782 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0782)

#||
(clif-logic
  (formal-problem "Project Euler 782: Complexity of an n x n binary matrix")
  (invariants
    (complexity-bound (always (<= (c n k) 4)))
    (complexity-1 (iff (<= (c n k) 1) (or (= k 0) (= k n^2))))
    (complexity-2 (iff (<= (c n k) 2) (subset-sum-p k (symmetric-2x2-blocks x (- n x)))))
    (complexity-3-families
      (implies (or (subset-sum-p k (symmetric-3x3-blocks x y z))
                   (= k (* x y)) (= k (- n^2 (* x y)))
                   (= k (+ (* x y) (* y z) (* z x)))
                   (= k (- n^2 (+ (* x y) (* y z) (* z x)))))
               (<= (c n k) 3))))
  (optimizations
    (pure-mathematics "Removed all Lisp-specific micro-optimizations. Correctness relies entirely on the exhaustive classification of complexity <= 3 matrices.")
    (missing-family-fixed "Added the mathematically overlooked 3-cycle asymmetric block matrices (k = xy+yz+zx).")))
||#

(defun solve ()
  (let* ((n 10000)
         (n2 (* n n))
         ;; Initialize with maximum possible complexity 4.
         ;; :element-type is used solely for reasonable memory footprint (100MB instead of 8GB).
         (c (make-array (1+ n2) :element-type '(unsigned-byte 8) :initial-element 4)))
    
    ;; Complexity 1
    (setf (aref c 0) 1)
    (setf (aref c n2) 1)
    
    ;; Complexity <= 2
    (iterate (for x from 1 below n)
      (let* ((nx (- n x))
             (k1 (* x x))
             (k2 (* nx nx))
             (k3 (* 2 x nx))
             (k4 (+ k1 k2))
             (k5 (+ k1 k3))
             (k6 (+ k2 k3)))
        (when (> (aref c k1) 2) (setf (aref c k1) 2))
        (when (> (aref c k2) 2) (setf (aref c k2) 2))
        (when (> (aref c k3) 2) (setf (aref c k3) 2))
        (when (> (aref c k4) 2) (setf (aref c k4) 2))
        (when (> (aref c k5) 2) (setf (aref c k5) 2))
        (when (> (aref c k6) 2) (setf (aref c k6) 2))))
        
    ;; Complexity <= 3 (Asymmetric 2x2 Rectangles)
    ;; Loop optimized to y <= x due to x*y symmetry.
    (iterate (for x from 1 to n)
      (iterate (for y from 1 to x)
        (let* ((k (* x y))
               (comp (- n2 k)))
          (when (> (aref c k) 3) (setf (aref c k) 3))
          (when (> (aref c comp) 3) (setf (aref c comp) 3)))))
          
    ;; Complexity <= 3 (Symmetric 3x3 Blocks and Asymmetric 3-cycles)
    (iterate (for x from 1 to (floor n 3))
      (iterate (for y from x to (floor (- n x) 2))
        (let* ((z (- n x y))
               (x2 (* x x))
               (y2 (* y y))
               (z2 (* z z))
               (2xy (* 2 x y))
               (2yz (* 2 y z))
               (2zx (* 2 z x)))
          
          ;; The newly discovered Asymmetric 3-cycle family
          (let* ((k-cycle (+ (* x y) (* y z) (* z x)))
                 (comp (- n2 k-cycle)))
            (when (> (aref c k-cycle) 3) (setf (aref c k-cycle) 3))
            (when (> (aref c comp) 3) (setf (aref c comp) 3)))

          ;; Symmetric 3x3 subset sums
          (iterate (for mask from 0 to 63)
            (let ((sum 0))
              (when (> (logand mask 1) 0) (incf sum x2))
              (when (> (logand mask 2) 0) (incf sum y2))
              (when (> (logand mask 4) 0) (incf sum z2))
              (when (> (logand mask 8) 0) (incf sum 2xy))
              (when (> (logand mask 16) 0) (incf sum 2yz))
              (when (> (logand mask 32) 0) (incf sum 2zx))
              (when (> (aref c sum) 3)
                (setf (aref c sum) 3)))))))
                
    ;; Sum the array
    (let ((total 0))
      (iterate (for k from 0 to n2)
        (incf total (aref c k)))
      total)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =       28.405
System time  =        0.244
Elapsed time =       28.497
Allocation   = 100283712 bytes
24711 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 318313204
:ok