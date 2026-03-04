;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: mercury-2
(cl:in-package cl-user)
(defpackage #:project-euler-206
  (:use #:cl #:alexandria #:iterate))
(in-package #:project-euler-206)

#||
;;====================================================================
;; Common Logic (CLIF) formalisation of Project Euler 206
;;====================================================================
;; 1.  Domain
;;    - Object  ?n   : candidate integer (positive)
;;    - Object  ?s   : square of ?n
;;    - Object  ?p   : pattern 1_2_3_4_5_6_7_8_9_0
;;    - Object  ?d_i : digit placeholder (i = 0..9)
;; 2.  Predicates
;;    (candidate ?n)                – ?n is a positive integer
;;    (square ?n ?s)                – ?s = ?n * ?n
;;    (pattern ?p)                  – ?p matches the digit‑template
;;    (digit ?d_i)                  – each placeholder is a single decimal digit
;;    (ends-with-zero ?s)           – last digit of ?s is 0
;;    (ends-with-nine ?s)           – last digit of ?s is 9 (after removing trailing 0)
;;    (matches-template ?s)         – ?s has the form 1_2_3_4_5_6_7_8_9_0
;; 3.  Axioms
;;    (forall (?n)
;;      (=> (candidate ?n)
;;          (and (square ?n ?s)
;;               (ends-with-zero ?s)
;;               (matches-template ?s))))
;;    (forall (?s)
;;      (=> (matches-template ?s)
;;          (exists (?d0 ?d1 ?d2 ?d3 ?d4 ?d5 ?d6 ?d7 ?d8 ?d9)
;;            (and (digit ?d0) (digit ?d1) (digit ?d2) (digit ?d3) (digit ?d4)
;;                 (digit ?d5) (digit ?d6) (digit ?d7) (digit ?d8) (digit ?d9)
;;                 (= ?s (concatenate
;;                        (list ?d0 ?d1 ?d2 ?d3 ?d4 ?d5 ?d6 ?d7 ?d8 ?d9))))))
;;    (forall (?n ?s)
;;      (=> (square ?n ?s)
;;          (= ?s (* ?n ?n))))
;; 4.  Goal
;;    (exists (?n)
;;      (=> (candidate ?n)
;;          (and (square ?n ?s)
;;               (matches-template ?s))
;;          (unique ?n))))
;;====================================================================
||#

;; ------------------------------------------------------------
;; Project Euler 206 solution (Common Lisp, using ITERATE)
;; ------------------------------------------------------------
;; The square must be of the form 1?2?3?4?5?6?7?8?9?0.
;; Because the last digit is 0, the root ends with 0.
;; Let n = 10·m, then m² must be of the form 1?2?3?4?5?6?7?8?9?.
;; The last digit of m² is 9, therefore m ends with 3 or 7.
;; The search interval for m is:
;;   sqrt(10¹⁶) ≤ m ≤ sqrt(2·10¹⁷)  (≈ 10⁸ … 1.5·10⁸)
;; We'll iterate over m with step 10, checking only the two possible
;; endings (3 and 7).  The first m that satisfies the pattern yields
;; the unique answer n = 10·m.
;; ------------------------------------------------------------

(defun matches-pattern-p (n)
  "Return true iff N² has the decimal pattern 1?2?3?4?5?6?7?8?9?0."
  (let* ((sq (the (unsigned-byte 128) (* n n)))
         (str (write-to-string sq)))
    (and (= (length str) 19)               ; exactly 19 digits
         (char= (char str 0) #\1)
         (char= (char str 2) #\2)
         (char= (char str 4) #\3)
         (char= (char str 6) #\4)
         (char= (char str 8) #\5)
         (char= (char str 10) #\6)
         (char= (char str 12) #\7)
         (char= (char str 14) #\8)
         (char= (char str 16) #\9)
         (char= (char str 18) #\0))))

(defun solve-euler-206 ()
  "Compute the unique integer whose square matches the required pattern."
  (let* ((lower (floor (sqrt (expt 10 16))))   ; sqrt(10¹⁶) = 10⁸
         (upper (floor (sqrt (* 2 (expt 10 17))))) ; sqrt(2·10¹⁷)
         (answer nil))
    (iterate
      (for m from lower to upper by 10)          ; step 10, keep last digit 0
      (for m1 = (+ m 3))                         ; candidate ending with 3
      (for m2 = (+ m 7))                         ; candidate ending with 7
      (when (matches-pattern-p (* 10 m1))
        (setf answer (* 10 m1))
        (return))
      (when (matches-pattern-p (* 10 m2))
        (setf answer (* 10 m2))
        (return)))
    answer))

;; ------------------------------------------------------------
;; Execution entry point
;; ------------------------------------------------------------
(defun main ()
  (format t "~&The solution to Project Euler 206 is: ~A~%" (solve-euler-206)))

;; Uncomment the following line to run automatically when the file is loaded:
;; (main)

;; ------------------------------------------------------------
;; Self‑analysis
;; ------------------------------------------------------------
;; The CLIF fragment above captures the essential logical structure
;; of the problem: a candidate integer n, its square s, and the
;; digit‑template constraint.  By making the “matches‑template”
;; predicate explicit, the formalisation forces the implementation
;; to treat the pattern as a series of positional digit checks rather
;; than a vague “string contains the pattern”.  This guided the
;; Lisp code to:
;;   • Reduce the search space by exploiting the trailing zero (n = 10·m).
;;   • Further reduce by the last‑digit property (m ends with 3 or 7).
;;   • Use a fixed‑length string comparison that mirrors the
;;     positional constraints expressed in CLIF.
;; The explicit quantifiers and existential digit placeholders in CLIF
;; also reminded me to verify the exact length (19 digits) and the
;; exact positions of the fixed digits, preventing off‑by‑one errors.
;; Consequently, the generated code follows a mathematically
;; justified search strategy rather than a naïve brute force,
;; which would be infeasible for the 10⁸‑scale interval.
;; The analysis therefore had a direct, positive impact on both
;; correctness and efficiency of the final solution.


#+| Do it | (main )
#|------------------------------------------------------------|
Timing the evaluation of (main)
The solution to Project Euler 206 is: 1389019170

User time    =       13.257
System time  =        0.187
Elapsed time =       13.779
Allocation   = 1012694464 bytes
5036 Page faults
GC time      =        0.010
 |------------------------------------------------------------|#
;;→ nil
:ok