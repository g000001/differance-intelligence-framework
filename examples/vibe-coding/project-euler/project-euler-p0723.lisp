;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0723 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0723)

#||
(clif-logic
  (formal-problem "Project Euler 723: Pythagorean Quadrilaterals")
  (invariants
    (exact-algebraic-geometry-collapse
      (equal (f (sqrt d))
             (+ (* 4 (S2 d)) (* -4 (S1 d)) (* 8 (expt (K d) 3)) (* -14 (expt (K d) 2)) (* 7 (K d)))))
    (multiplicative-basis
      (and (equal (K d) (prod_{p^x || d} (+ x 1)))
           (equal (S2 d) (prod_{p^x || d} (/ (* (+ x 1) (+ (* 2 x x) (* 4 x) 3)) 3)))
           (equal (S1 d) (prod_{p^x || d} (floor (+ (expt (+ x 1) 2) 1) 2))))))
  (optimizations
    (pure-mathematics "Removed all optimization declarations. The algorithm complexity is strictly bounded by the number of divisors (2688 operations).")
    (zero-allocation "Recursion only uses stack variables for accumulating products. Completely silences the GC.")))
||#

(defun solve ()
  "Evaluates S(n) using the exact derived multiplicative functions."
  (let ((exponents '(6 3 2 1 1 1 1 1))
        (total-sum 0))
    
    (labels ((recurse (idx current-k current-s2 current-s1)
               (if (= idx (length exponents))
                   ;; Base case: all prime factor exponents chosen. Evaluate f(sqrt(d)).
                   (let* ((k current-k)
                          (s2 current-s2)
                          (s1 current-s1)
                          (k2 (* k k))
                          (k3 (* k2 k))
                          (f (+ (* 4 s2)
                                (- (* 4 s1))
                                (* 8 k3)
                                (- (* 14 k2))
                                (* 7 k))))
                     (incf total-sum f))
                   
                   ;; Iterate through all possible exponents x for the current prime factor
                   (let ((max-e (nth idx exponents)))
                     (iterate (for x from 0 to max-e)
                       (let ((next-k (* current-k (+ x 1)))
                             (next-s2 (* current-s2 (/ (* (+ x 1) (+ (* 2 x x) (* 4 x) 3)) 3)))
                             (next-s1 (* current-s1 (floor (+ (* (+ x 1) (+ x 1)) 1) 2))))
                         (recurse (1+ idx) next-k next-s2 next-s1)))))))
      
      ;; Start recursion with initial multiplicative bases = 1
      (recurse 0 1 1 1))
      
    total-sum))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 0 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1395793419248
:ok