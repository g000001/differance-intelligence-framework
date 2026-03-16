;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler--1 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler--1)

#||
(cl:comment "CLIF logic for Project Euler -1 (Infinity Variation)")
(cl:text
(Constraint "Summation up to infinity implies Ramanujan summation / Zeta function regularization")
(Equivalence (Summation n 1 infinity n) -1/12)
(Equivalence (Summation n 1 infinity (* k n)) (* k -1/12))

(cl:comment "Inclusion-Exclusion Principle")
(Target (= ?answer (- (+ (* 3 -1/12) (* 5 -1/12)) (* 15 -1/12))))

(Optimization "O(1) calculation using native rational numbers, avoiding infinite loops entirely")
)
||#


(defun solve ()
  "Calculates the Ramanujan summation of all multiples of 3 or 5 below infinity."
  (let* ((riemann-zeta-at-minus-one -1/12)
         ;; Calculate the regularized sum of multiples
         (regularized-sum-of-multiples-of-three (* 3 riemann-zeta-at-minus-one))
         (regularized-sum-of-multiples-of-five (* 5 riemann-zeta-at-minus-one))
         (regularized-sum-of-multiples-of-fifteen (* 15 riemann-zeta-at-minus-one)))
    
    ;; Outermost print debug to observe the logical flow
    (format t "debug: Zeta regularization baseline sum = ~A~%" riemann-zeta-at-minus-one)
    (format t "debug: Applying Inclusion-Exclusion principle...~%")
    
    ;; Return the result using exact rational arithmetic (Ratio type in Common Lisp)
    (- (+ regularized-sum-of-multiples-of-three 
          regularized-sum-of-multiples-of-five)
       regularized-sum-of-multiples-of-fifteen)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
debug: Zeta regularization baseline sum = -1/12
debug: Applying Inclusion-Exclusion principle...

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 280 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 7/12
:ok