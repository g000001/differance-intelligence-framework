;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0964 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0964)

#||
(cl:comment "CLIF logic for Project Euler 964")
(cl:text
 (Constraint "Avoid direct permutation simulation (22! states)")
 (Invariant "The round operations commute in the center of the group algebra C[S_N]")
 (Invariant "The target permutation is an N-cycle, whose characters are non-zero ONLY on hook-shaped partitions")
 (Equivalence "Pieri's rule reduces the eigenvalue of the round operation on hook representation r to proportional binomial coefficients")
 (Optimization "The probability equation simplifies to a sum of N-k terms, computing exact rationals in O(k N) time")
 )
||#

(defun compute-choose (n k)
  "Calculates the binomial coefficient (n choose k)."
  (if (or (< k 0) (> k n))
      0
      (let ((result-val 1))
        (iterate (for i from 1 to k)
          (setf result-val (/ (* result-val (- n (1- i))) i)))
        result-val)))

(defun compute-factorial (n)
  "Calculates the factorial of n."
  (let ((result-val 1))
    (iterate (for i from 1 to n)
      (setf result-val (* result-val i)))
    result-val))

(defun format-scientific (rational-num)
  "Formats a rational number into scientific notation with 10 significant digits after the decimal point."
  ;; Find the initial exponent E such that 1 <= num / 10^E < 10
  (let ((e-val (floor (log (coerce rational-num 'double-float) 10d0))))
    (iterate
      (let ((scaled-val (* rational-num (expt 10 (- e-val)))))
        (cond ((>= scaled-val 10) (incf e-val))
              ((< scaled-val 1) (decf e-val))
              (t (finish)))))
    
    ;; Scale the number to extract exactly 11 digits (1 before decimal, 10 after)
    (let* ((scaled-val (* rational-num (expt 10 (- e-val))))
           (rounded-val (round (* scaled-val 10000000000))))
      
      ;; Handle edge case where rounding pushes the number up to the next power of 10
      (when (>= rounded-val 100000000000)
        (setf rounded-val (round (/ rounded-val 10)))
        (incf e-val))
        
      (let ((val-str (format nil "~D" rounded-val)))
        (format nil "~A.~Ae~A"
                (subseq val-str 0 1)
                (subseq val-str 1)
                e-val)))))

(defun solve ()
  "Calculates P(7), the probability of everyone sitting exactly one chair to the right."
  (let* ((target-k 7)
         (total-n (1+ (/ (* target-k (1- target-k)) 2)))
         (total-probability 0))
    
    (format t "debug: Computing sum for k=~A, N=~A...~%" target-k total-n)
    
    ;; The mathematical shortcut: sum over the N hook representations
    ;; Effectively, the sum truncates at N-k because compute-choose(N-i, r) becomes 0.
    (iterate (for r from 0 below total-n)
      (let ((term-sign (if (evenp r) 1 -1))
            (term-base-choose (compute-choose (1- total-n) r))
            (term-product 1))
        
        ;; Compute the product of eigenvalues for each round i = 1 to k
        (iterate (for i from 1 to target-k)
          (let ((numerator (compute-choose (- total-n i) r))
                (denominator term-base-choose))
            (setf term-product (* term-product (/ numerator denominator)))))
            
        (let ((final-term (* term-sign term-base-choose term-product)))
          (incf total-probability final-term))))
          
    ;; Divide by N! as per the class function coefficient extraction
    (setf total-probability (/ total-probability (compute-factorial total-n)))
    
    (format t "debug: Exact rational probability = ~A~%" total-probability)
    (format-scientific total-probability)))



#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
debug: Computing sum for k=7, N=22...
debug: Exact rational probability = 1/21219647838989618324275200000

User time    =        0.000
System time  =        0.000
Elapsed time =        0.001
Allocation   = 6648 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "4.7126135532e-29"
:ok
