;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0855 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0855)

#||
(clif-logic
  (formal-problem "Project Euler 855: Paper-cutting continuous game")
  (invariants
    (minimax-fair-division
      (equal (W U) (/ (expt (factorial (length U)) 2)
                      (* (prod_{i} (factorial R_i))
                         (prod_{j} (factorial C_j))))))
    (global-solution
      (equal (S a b) (/ (* (expt (factorial b) a) (expt (factorial a) b))
                        (expt (factorial (* a b)) 2)))))
  (optimizations
    (absolute-dimension-collapse "The entire continuous game tree analytically collapses into a single closed-form combinatorial invariant. No DP, no arrays, no GC.")
    (exact-bignum-arithmetic "Calculates S(5,8) entirely using exact rational arithmetic, bypassing any float inaccuracy.")
    (bulletproof-formatter "Formats the exact rational to 10 decimal digits using precise integer scaling and rounding.")))
||#

(defun factorial (n)
  (let ((res 1))
    (iterate (for i from 1 to n)
      (setf res (* res i)))
    res))

(defun format-scientific-exact (a b)
  "Strictly computes scientific notation from an exact rational a/b.
   Guarantees 10 decimal digits of precision by scaling with exact bignum integers."
  (let* ((approx-float (/ (coerce a 'double-float) (coerce b 'double-float)))
         (p (floor (log approx-float 10.0d0)))
         (scaled-num (* a (expt 10 (- 10 p))))
         (r (round scaled-num b)))
    ;; Adjust power if banker's rounding rolls over the 10-digit boundary
    (when (>= r 100000000000)
      (setf r 10000000000)
      (incf p))
    (when (< r 10000000000)
      (setf scaled-num (* a (expt 10 (- 11 p))))
      (setf r (round scaled-num b))
      (decf p))
    (let ((s (format nil "~D" r)))
      (format nil "~A.~Ae~A" (subseq s 0 1) (subseq s 1) p))))

(defun solve ()
  "Evaluates S(5,8) using the true combinatorial invariant."
  (let* ((a 5)
         (b 8)
         (n (* a b))
         ;; S(a,b) = ( (b!)^a * (a!)^b ) / ((ab)!)^2
         (num (* (expt (factorial b) a) (expt (factorial a) b)))
         (den (expt (factorial n) 2)))
    (format-scientific-exact num den)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 1344 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "6.8827571976e-57"
:ok