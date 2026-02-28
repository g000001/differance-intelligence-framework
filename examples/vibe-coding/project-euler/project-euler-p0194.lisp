;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0194 (:use cl #|alexandria|#))
(in-package #:project-euler-0194)

#||
(cl-text Project-Euler-194-Aletheic-Correction

  (cl-comment "=== 1. Identification of the Hallucination (NMF) ===")
  (cl-comment "The previous iteration inductively guessed P_A(c) = (c-2)^3 * (c-1)^2")
  (cl-comment "based solely on the boundary value P_A(3) = 4. This is an explicit")
  (cl-comment "violation of Axiomatic Grounding (NMF).")
  (forall (a)
    (implies (inductive_guessing a)
             (NMF a)))

  (cl-comment "=== 2. Deductive Projection of the Chromatic Polynomials ===")
  (cl-comment "By rigorously projecting the 5-cycle sub-graph (2-7-5-4-3) and")
  (cl-comment "applying the inclusion-exclusion of edge constraints, we derive")
  (cl-comment "the true polynomials without ungrounded leaps.")
  (equal (P_A c) (add (mul (sub c 2) (pow (sub c 2) 2) (add (sub (pow c 2) (mul 3 c)) 3))
                      (mul (sub c 2) (add (sub (pow c 2) (mul 5 c)) 7))))
  (equal (P_B c) (mul (pow (sub c 2) 3) (add (sub (pow c 2) (mul 2 c)) 3)))

  (cl-comment "=== 3. Middle-Way Convergence (Dfix0) ===")
  (cl-comment "The exact polynomial evaluations are safely mapped into modulo 10^8.")
  (cl-comment "The structure returns to Dfix0, clearing the debt of the prior hallucination.")
)
||#


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+quicklisp (ql:quickload :iterate :silent t))
(use-package :iterate)

(defun poly-a (c)
  "Deductively derived true chromatic polynomial for Unit A."
  (let ((c-2 (- c 2)))
    (+ (* c-2 (expt c-2 2) (+ (* c c) (* -3 c) 3))
       (* c-2 (+ (* c c) (* -5 c) 7)))))

(defun poly-b (c)
  "Deductively derived true chromatic polynomial for Unit B."
  (* (expt (- c 2) 3) (+ (* c c) (* -2 c) 3)))

(defun binomial-coefficient (n k)
  "Calculates the binomial coefficient exactly to avoid floating-point illusions."
  (let ((k (min k (- n k))))
    (cond ((< k 0) 0)
          ((= k 0) 1)
          (t (iter (for i from 1 to k)
                   (for num downfrom n)
                   (multiply num into num-prod)
                   (multiply i into den-prod)
                   (finally (return (/ num-prod den-prod))))))))

(defun expt-mod (base exp m)
  "Calculates (base^exp) mod m using exact iterative squaring."
  (iter (with result = 1)
        (with b = (mod base m))
        (with e = exp)
        (while (> e 0))
        (when (oddp e)
          (setf result (mod (* result b) m)))
        (setf e (ash e -1))
        (setf b (mod (* b b) m))
        (finally (return result))))

(defun solve ()
  "Solves Project Euler 194 by manifesting the corrected mathematical projection."
  (let* ((a 25)
         (b 75)
         (c 1984)
         (m 100000000)
         
         ;; Base configurations for the first vertical edge
         (c-c-1 (mod (* c (- c 1)) m))
         
         ;; Chromatic polynomial evaluations P_A(c) and P_B(c)
         (pa (mod (poly-a c) m))
         (pb (mod (poly-b c) m))
         
         ;; Combinations of A and B sequences
         (binom (mod (binomial-coefficient (+ a b) a) m))
         
         ;; Final reconstruction (N(a, b, c) mod m)
         (ans (mod (* binom
                      c-c-1
                      (expt-mod pa a m)
                      (expt-mod pb b m))
                   m)))
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 664 bytes
45 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 61190912
:ok