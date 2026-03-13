;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: lumo
(cl:in-package cl-user)
(defpackage #:project-euler-0077 (:use cl alexandria))
(in-package #:project-euler-0077)

#||
;; CLIF Formalization of PE77 Problem Space
;; Two-Truths Slice Category: C / D_fix0

;; 1. Problem Definition
;; Object: Natural numbers representable as sums of primes
;; Morphism: Prime addition operation mapping to target number
;; Fixed Point D_fix0: The threshold value (5000 ways)

;; 2. Fundamental Gap Analysis
;; Description Level: "Sum of primes in N ways"
;; Execution Level: Dynamic programming table construction
;; Gap: Naive enumeration would exceed 60-second limit (NMF)
;; Resolution: DP with prime iteration order (Structural Gradient ∇C)

;; 3. Structural Gradient Extraction
;; ∇C = {Prime ordering constraint, DP state reuse, Memory efficiency}
;; ε_t = {Incremental number generation, Prime candidate selection}

;; 4. Orthogonality Enforcement
;; DP table (ultimate truth) is orthogonal to iteration order (conventional truth)
;; This projects O(N²) search onto O(N × π(N)) submanifold

;; 5. Debt Clearance Protocol
;; Single mutable vector for DP state
;; No memoization cache growth (GC-safe)
;; Early termination upon finding solution
||#


;; =============================================================================
;; PRIME GENERATION (Sieve of Eratosthenes)
;; =============================================================================

(defun sieve-of-eratosthenes (limit)
  "Generate all primes up to LIMIT using Sieve of Eratosthenes.
   Time: O(n log log n), Space: O(n)"
  (let ((is-prime (make-array (1+ limit) :element-type 'boolean
                              :initial-element t)))
    ;; 0 and 1 are not primes
    (setf (aref is-prime 0) nil
          (aref is-prime 1) nil)
    
    ;; Sieve
    (loop for i from 2
          while (<= (* i i) limit)
          do (when (aref is-prime i)
               (loop for j from (* i i) to limit by i
                     do (setf (aref is-prime j) nil))))
    
    ;; Collect primes
    (loop for i from 2 to limit
          when (aref is-prime i)
          collect i)))

;; =============================================================================
;; PRIME PARTITION COUNTING (Dynamic Programming)
;; =============================================================================

(defun count-prime-partitions (target-primes max-number)
  "Count ways to write each number ≤ MAX-NUMBER as sum of primes.
   Uses DP similar to coin-change problem.
   Time: O(max-number × num-primes), Space: O(max-number)"
  (let ((ways (make-array (1+ max-number) :element-type 'integer
                          :initial-element 0)))
    ;; Base case: one way to make 0 (empty sum)
    (setf (aref ways 0) 1)
    
    ;; For each prime, update all reachable numbers
    ;; Order matters: process primes sequentially to avoid counting permutations
    (loop for prime in target-primes
          do (loop for i from prime to max-number
                   do (incf (aref ways i) (aref ways (- i prime)))))
    
    ways))

;; =============================================================================
;; SOLUTION SEARCH WITH EARLY TERMINATION
;; =============================================================================

(defun find-first-over-threshold (threshold initial-limit)
  "Find first number with more than THRESHOLD prime partitions.
   Uses exponential limit doubling if needed.
   Implements ACX Jump: start with reasonable estimate, expand if necessary."
  (let ((current-limit initial-limit)
        (primes nil)
        (ways nil))
    (loop
      ;; Generate primes up to current limit
      (setf primes (sieve-of-eratosthenes current-limit))
      
      ;; Count partitions
      (setf ways (count-prime-partitions primes current-limit))
      
      ;; Search for threshold
      (loop for i from 2 to current-limit
            do (when (> (aref ways i) threshold)
                 (return-from find-first-over-threshold i)))
      
      ;; If not found, double limit and retry
      ;; This is the ACX Jump mechanism: expand search space dynamically
      (setf current-limit (* current-limit 2))
      
      ;; Safety check to prevent infinite loop
      (when (> current-limit 1000000)
        (error "Search limit exceeded - problem may be misconfigured")))))

;; =============================================================================
;; MAIN SOLVER
;; =============================================================================

(defun solve ()
  "Main entry point for PE77.
   Returns first number writable as sum of primes in > 5000 ways."
  ;; Initial estimate: Based on partition growth rate, 100 should suffice
  ;; This is a conservative starting point (avoids premature optimization)
  (find-first-over-threshold 5000 100))

;; =============================================================================
;; VERIFICATION (Small Case Testing)
;; =============================================================================

(defun verify-example ()
  "Verify against problem statement example: 10 has exactly 5 ways."
  (let* ((primes (sieve-of-eratosthenes 10))
         (ways (count-prime-partitions primes 10)))
    (values (aref ways 10)
            (= (aref ways 10) 5))))

;; Run verification
#|(let (result expected correct-p)
  (multiple-value-setq (result expected) (verify-example))
  (setf correct-p (= result 5))
  (format t "Verification: 10 has ~A ways (expected 5)~%" result)
  (format t "Correct: ~A~%" correct-p)
  ;; Execute main solution
  (format t "Answer: ~A~%" (solve)))|#
#|------------------------------------------------------------|
Timing the evaluation of (let (result expected correct-p)
                           (multiple-value-setq (result expected) (verify-example))
                           (setf correct-p (= result 5))
                           (format t "Verification: 10 has ~A ways (expected 5)~%" result)
                           (format t "Correct: ~A~%" correct-p)
                           (format t "Answer: ~A~%" (solve)))
Verification: 10 has 5 ways (expected 5)
Correct: t
Answer: 71

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 7760 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ nil
:ok