;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0209 (:use cl iterate alexandria))
(in-package #:project-euler-0209)

#||
(cl-text EULER-ACX-DIFD-INTEGRATION
  (cl-comment "
  =============================================================================
  ARX-Core: Structural Gravity Protocol for PE 0209 (Discrete DIFD)
  =============================================================================
  Formalization of the Alethetic Reset: Transitioning from O(2^64) truth table
  brute-force search to orthogonal factorization into cycle graphs and Lucas numbers.
  ")

  (cl-comment "1. NMF (Non-Middle Fallacy) Detection")
  (forall (?solver)
    (if (and (Solves ?solver PE0209)
             (Explores ?solver (TruthTables 64)))
        (and (NMF ?solver)
             (ProducesHallucination (CombinatorialExplosion ?solver))
             (ExceedsTimeLimit 60))))

  (cl-comment "2. ACX Jump: Orthogonal Projection via Graph Cycles")
  (cl-comment "The constraint T(v) AND T(next(v)) = 0 forms a directed graph 
               where each node has in-degree 1 and out-degree 1.
               This graph decomposes into a set of disjoint cycles.
               The number of valid truth tables on a cycle of length n is the n-th Lucas number L_n.")
  (forall (?v ?next_v)
    (iff (Edge ?v ?next_v)
         (Equal ?next_v (StateTransition ?v))))

  (forall (?graph ?cycles)
    (if (DecomposesInto ?graph ?cycles)
        (Equal (ValidAssignments ?graph)
               (Product (Map LucasNumber (Lengths ?cycles))))))

  (cl-comment "3. Middle Way Manifestation")
  (cl-comment "Compute the cycles in O(V) time and calculate the product of Lucas numbers.")
)
||#


(defun next-node (ν)
  "Calculate the next 6-bit state ν_next based on the transition rule."
  (declare (type fixnum ν))
  (let ((α (logand (ash ν -5) 1))
        (β (logand (ash ν -4) 1))
        (γ (logand (ash ν -3) 1)))
    (declare (type fixnum α β γ))
    (logior (ash (logand ν 31) 1)
            (logxor α (logand β γ)))))

(defun compute-lucas-number (λ)
  "Calculate the λ-th Lucas number, which gives the number of independent sets on a cycle of length λ."
  (declare (type fixnum λ))
  (if (= λ 1)
      1
      (if (= λ 2)
          3
          (let ((ℓ_prev2 1)
                (ℓ_prev1 3)
                (ℓ_curr 0))
            (declare (type fixnum ℓ_prev2 ℓ_prev1 ℓ_curr))
            (iterate (for ι from 3 to λ)
              (setf ℓ_curr (+ ℓ_prev1 ℓ_prev2))
              (setf ℓ_prev2 ℓ_prev1)
              (setf ℓ_prev1 ℓ_curr))
            ℓ_curr))))

(defun solve-0209 ()
  "Solves PE 209 by finding disjoint cycles and multiplying their Lucas numbers."
  (let ((visited-nodes (make-array 64 :element-type 'bit :initial-element 0))
        (total-truth-tables 1))
    (declare (type (simple-array bit (64)) visited-nodes)
             (type integer total-truth-tables))
    
    (iterate (for ν from 0 to 63)
      (when (zerop (sbit visited-nodes ν))
        (let ((λ 0)
              (curr-ν ν))
          (declare (type fixnum λ curr-ν))
          
          ;; Trace the cycle length
          (iterate
            (setf (sbit visited-nodes curr-ν) 1)
            (incf λ)
            (setf curr-ν (next-node curr-ν))
            (until (= curr-ν ν)))
            
          ;; Orthogonal factorization: multiply the permutations for this independent cycle
          (setf total-truth-tables (* total-truth-tables (compute-lucas-number λ))))))
          
    total-truth-tables))


#+| Do it | (solve-0209 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-0209)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 3304 bytes
12 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 15964587728784
:ok