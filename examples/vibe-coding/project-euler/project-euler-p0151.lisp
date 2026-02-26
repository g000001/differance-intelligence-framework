;; ==========================================================
;; SKDT – Dual Sunyata Structures and Project Euler 0151
;; --------------------------------------------------------------
;; Title   : Expected Single-Sheet Encounters in Paper Cutting
;; Author  : Masaomi Chiba
;; Date    : 2025-12-01
;; Version : 1.0
;; --------------------------------------------------------------

#||
(cl-comment "Metadata")
(cl-comment "Title   : Expected Single-Sheet Encounters in Paper Cutting")
(cl-comment "Author  : Masaomi Chiba")
(cl-comment "Date    : 2025-12-01")
(cl-comment "Version : 1.0")

(cl-text
  (cl-comment "Analysis of Problem 0151 using Common Logic (CLIF)")

  ;; 1. The state of the envelope is a multiset of paper sizes {A1, A2, A3, A4, A5}.
  ;; We represent this as a vector (n1, n2, n3, n4, n5).
  (forall (?n1 ?n2 ?n3 ?n4 ?n5)
    (iff (EnvelopeState ?n1 ?n2 ?n3 ?n4 ?n5)
         (and (integer ?n1) (integer ?n2) (integer ?n3) (integer ?n4) (integer ?n5))))

  ;; 2. Cutting Procedure: Picking a sheet of size Ai (i < 5) results in Ai-1, and 
  ;; adding sheets of size Ai+1, ..., A5 to the envelope.
  (forall (?i ?n_i)
    (if (and (picked ?i) (less_than ?i 5))
        (and (decrement ?n_i)
             (forall (?j) (if (and (greater_than ?j ?i) (less_equal ?j 5))
                              (increment ?n_j))))))

  ;; 3. Transition Probability: Probability of picking Ai is n_i / TotalSheets.
  (forall (?i ?total)
    (= (ProbPick ?i) (div (count ?i) ?total)))

  ;; 4. Target Event: The supervisor finds a single sheet (TotalSheets = 1).
  (definition (SingleSheetFound ?total)
    (iff (SingleSheetFound ?total) (equal ?total 1)))

  ;; 5. Goal: Expected value of SingleSheetFound over 16 batches, 
  ;; excluding the first (Batch 1) and last (Batch 16).
  ;; Initial state after Batch 1: (0, 1, 1, 1, 1).
  ;; Termination state: (0, 0, 0, 0, 1) before Batch 16.
  (ExpectedValue (SumOverBatches (range 2 15) (Prob (SingleSheetFound 1))))
)
||#

;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview

(cl:in-package cl-user)
(defpackage #:project-euler-0151 
  (:use #:cl #:iterate #:alexandria))
(in-package #:project-euler-0151)

(defparameter *memo* (make-hash-table :test 'equal))

(defun solve (n2 n3 n4 n5)
  "Recursively calculates the expected number of single-sheet encounters.
   n2..n5 represent counts of A2..A5 sheets. A1 is already used in Batch 1."
  (let* ((state (list n2 n3 n4 n5))
         (total (+ n2 n3 n4 n5)))
    ;; Check memoization table
    (multiple-value-bind (res exists) (gethash state *memo*)
      (if exists
          res
          (setf (gethash state *memo*)
                (cond
                  ;; Base case: One A5 left. This is the start of Batch 16.
                  ;; Per problem, we exclude the last batch, so return 0.0.
                  ((and (= total 1) (= n5 1))
                   0.0d0)
                  
                  (t
                   (let ((expected-here (if (= total 1) 1.0d0 0.0d0)))
                     ;; Sum (Probability of picking Ai * Expected value of next state)
                     (+ expected-here
                        (iterate (for i from 2 to 5)
                                 (for count in state)
                                 (when (> count 0)
                                   (let ((prob (/ (float count 0.0d0) total))
                                         (next-n2 n2) (next-n3 n3) (next-n4 n4) (next-n5 n5))
                                     ;; Execute 'cut-in-half' procedure
                                     (case i
                                       (2 (decf next-n2) (incf next-n3) (incf next-n4) (incf next-n5))
                                       (3 (decf next-n3) (incf next-n4) (incf next-n5))
                                       (4 (decf next-n4) (incf next-n5))
                                       (5 (decf next-n5)))
                                     (sum (* prob (solve next-n2 next-n3 next-n4 next-n5)))))))))))))))

(defun main ()
  "Calculates the result for Project Euler 0151."
  ;; Clear memoization for fresh run
  (clrhash *memo*)
  ;; The supervisor starts with one A1. 
  ;; Batch 1: Picks A1, cuts to {A2, A3, A4, A5}.
  ;; We start our calculation from the state AFTER Batch 1.
  ;; The supervisor will find 1 sheet at Batch 1 (A1) and Batch 16 (A5).
  ;; Both are excluded by starting at (1,1,1,1) and returning 0 for (0,0,0,1).
  (let ((ans (solve 1 1 1 1)))
    (format t "~,6f~%" ans)
    ans))

;; Execute
;(main)

;; ==============================================================
;; End of File
;; --------------------------------------------------------------
;; The algorithm uses memoized recursion to traverse the state space of paper
;; counts. By defining the state after the first batch and terminating before 
;; the last, we satisfy the 'excluding first and last' constraint.
;; ==============================================================

#+| Do it | (main )
;▻ 0.464399
;→ 0.464398781601087D0


