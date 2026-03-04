;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: mercury-2
(cl:in-package cl-user)

(defpackage #:project-euler-0073
  (:use #:cl #:alexandria iterate))
(in-package #:project-euler-0073)

#| 
;; ============================================================
;; Common Logic (clif) analysis of Project Euler Problem 73
;; ============================================================

;; 1. Primitive notions
(object   ?n)                ;; numerator (positive integer)
(object   ?d)                ;; denominator (positive integer)
(object   ?limit)            ;; upper bound for denominators (positive integer)

;; 2. Predicates
(less-than ?n ?d)          ;; n < d
(hcf-1 ?n ?d)              ;; HCF(n,d) = 1  (coprime)
(proper-reduced ?n ?d)    ;; reduced proper fraction
(between-1/3-1/2 ?n ?d)    ;; 1/3 < n/d < 1/2

;; 3. Definitions
(define (proper-reduced ?n ?d)
  (and (less-than ?n ?d)
       (hcf-1 ?n ?d)))

(define (between-1/3-1/2 ?n ?d)
  (and (proper-reduced ?n ?d)
       (=> (<= ?n (floor (/ ?d 3)))   ; n > d/3
           (>= ?n (ceil (/ ?d 2)))    ; n < d/2
           (not (= ?n (floor (/ ?d 3))))
           (not (= ?n (ceil (/ ?d 2)))))))

;; 4. Counting problem
;; Count all pairs (n,d) with d ≤ limit, proper‑reduced,
;; and lying strictly between 1/3 and 1/2.
(define (count-fractions ?limit)
  (exists ((total integer))
    (and (equal total
                 (sum ((d integer) (1 ≤ d ≤ ?limit))
                      (sum ((n integer) (1 ≤ n ≤ d))
                           (if (between-1/3-1/2 n d) 1 0)))))
    total))

;; 5. Operationalisation for computation
;; For each denominator d we need the number of integers n such that
;;   floor(d/3) < n < ceil(d/2)   and   gcd(n,d)=1.
;; This is expressed as:
(define (coprime-count-in-interval ?d)
  (exists ((cnt integer))
    (and (equal cnt
                 (sum ((n integer)
                      (floor(d/3) < n < ceil(d/2)))
                      (if (= (gcd n ?d) 1) 1 0))))
    cnt))

;; The overall answer is then:
(define (answer ?limit)
  (exists ((ans integer))
    (and (equal ans
                 (sum ((d integer) (1 ≤ d ≤ ?limit))
                      (coprime-count-in-interval d))))
    ans))

;; ============================================================
;; End of clif analysis
;; ============================================================
|#

;; --------------------------------------------------------------------
;; Common Lisp implementation using the ITERATE library
;; --------------------------------------------------------------------
(defun count-fractions (limit)
  "Return the number of reduced proper fractions n/d with
   1/3 < n/d < 1/2 and 1 ≤ d ≤ LIMIT."
  (iterate
    (for d from 1 to limit)
    (summing
     (iterate
       (for n from (1+ (floor (/ d 3)))   ; smallest n > d/3
            to (floor (/ (1- d) 2)))        ; largest n < d/2
       (summing (if (= (gcd n d) 1) 1 0))))))

;; --------------------------------------------------------------------
;; Entry point for the specific Project Euler query (LIMIT = 12000)
;; --------------------------------------------------------------------
(defun solve-project-euler-73 ()
  (let ((limit 12000))
    (format t "Number of fractions between 1/3 and 1/2 for d ≤ ~A: ~A~%"
            limit (count-fractions limit))
    (count-fractions limit)))

;; --------------------------------------------------------------------
;; Self‑analysis
;; --------------------------------------------------------------------
;; The clif analysis above forced an explicit formalisation of the
;; problem: we introduced predicates for “proper‑reduced” and
;; “between‑1/3‑1/2”, and we expressed the counting task as a double
;; nested summation.  By translating that specification directly into
;; an ITERATE‑based program, the structure of the CLIF model guided the
;; implementation: the outer Σ over denominators became the outer
;; ITERATE, the inner Σ over admissible numerators became the inner
;; ITERATE, and the coprime‑condition was realised with a simple
;; (= (gcd n d) 1) test.  This close mapping kept the code faithful to
;; the logical description, avoided premature static shortcuts (the
;; “NMF” avoidance), and ensured that the algorithm remains a
;; dynamic exploration of the search space rather than a closed‑form
;; formula.  Consequently, the generated Lisp code is both
;; transparent and directly traceable back to its formal CLIF
;; specification.


#+| Do it | (solve-project-euler-73 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-project-euler-73)
Number of fractions between 1/3 and 1/2 for d ≤ 12000: 7295372

User time    =        1.368
System time  =        0.013
Elapsed time =        1.338
Allocation   = 1094976 bytes
1088 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 7295372
:ok