;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0239 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0239)

#||
(cl-text EULER-ACX-DIFD-INTEGRATION
  (cl-comment "
  =============================================================================
  ARX-Core: Structural Gravity Protocol for PE 0239 (Discrete DIFD)
  =============================================================================
  Formalization of the Alethetic Reset: Transitioning from O(N!) permutation 
  search to Orthogonal Projection via Inclusion-Exclusion.
  ")

  (cl-comment "1. NMF (Non-Middle Fallacy) Detection")
  (forall (?solver)
    (if (and (Solves ?solver PE0239)
             (Explores ?solver (Permutations 100)))
        (and (NMF ?solver)
             (ProducesHallucination (CombinatorialExplosion ?solver))
             (ExceedsTimeLimit 60))))

  (cl-comment "2. ACX Jump: Orthogonal Projection via Inclusion-Exclusion")
  (cl-comment "Instead of generating 100! permutations, project the constraints
               onto the prime elements. There are 25 primes <= 100.
               We fix exactly 3 primes in their natural positions: C(25, 3) ways.
               The remaining 22 primes MUST be deranged (displaced). 
               The 75 composite numbers can be placed anywhere.
               The number of such partial derangements is computed via the 
               Principle of Inclusion-Exclusion in O(K) time.")
  (forall (?n ?p ?k)
    (iff (ValidPartialDerangement ?n ?p ?k)
         (Equal (TotalWays)
                (* (Combination ?p (- ?p ?k))
                   (Sum (?j 0 ?k)
                        (* (^ -1 ?j)
                           (Combination ?k ?j)
                           (Factorial (- ?n (- ?p ?k) ?j))))))))

  (cl-comment "3. Middle Way Manifestation")
  (cl-comment "Compute the exact rational probability and extract the first 12 
               decimal places via Bignum arithmetic. Time complexity is O(P).")
)
||#

(defun combination (n k)
  "Calculate the binomial coefficient C(n, k)."
  (if (or (< k 0) (> k n))
      0
      (let ((c 1))
        (iterate (for i from 1 to k)
                 ;; Use floor to ensure integer arithmetic without creating ratios
                 (setf c (floor (* c (- (1+ n) i)) i)))
        c)))


(defun solve ()
  "Calculate the probability of the specific partial derangement."
  (let* ((τ_total 100)
         ;; Primes up to 100: 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97 (25 primes)
         (π_primes 25)
         (φ_fixed 3)
         (δ_deranged (- π_primes φ_fixed))
         (ω_ways-fixed (combination π_primes φ_fixed))
         (σ_sum 0))
    
    ;; Apply the Principle of Inclusion-Exclusion for the deranged primes
    (iterate (for j from 0 to δ_deranged)
             (let* ((sgn (if (evenp j) 1 -1))
                    (cmb (combination δ_deranged j))
                    ;; The remaining elements (75 composites + 22 primes - j matching primes) can be anywhere
                    (fac (factorial (- τ_total φ_fixed j)))
                    (term (* sgn cmb fac)))
               (incf σ_sum term)))
               
    (let* ((total-ways (* ω_ways-fixed σ_sum))
           ;; Keep the exact probability as a rational number (Ratio)
           (prob (/ total-ways (factorial τ_total)))
           ;; Shift decimal point by 12 places to prepare for rounding
           (scaled (* prob 1000000000000))
           ;; Round to nearest integer (handles banker's rounding if exactly .5, though impossible here)
           (rounded (round scaled)))
           
      ;; Format as "0." followed by exactly 12 digits, zero-padded if necessary
      (format nil "0.~12,'0D" rounded))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 72232 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "0.001887854841"
:ok