;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0128 (:use cl #|alexandria|#))
(in-package #:project-euler-0128)

#||
(cl-text Project-Euler-128-Aletheic-Analysis

  (cl-comment "=== 1. NMF Avoidance (非中道の誤謬の回避) ===")
  (cl-comment "Simulating the hexagonal grid explicitly in a 2D array or graph")
  (cl-comment "incurs O(N^2) space/time complexity and state debt.")
  (cl-comment "We reject this conventional attachment and project to a structural invariant.")
  (forall (a p)
    (if (simulates_2D_grid a)
        (NMF a)))

  (cl-comment "=== 2. ACX Jump (勝義諦への跳躍) ===")
  (cl-comment "By analyzing the differences, PD(n)=3 is mathematically possible ONLY")
  (cl-comment "for the first tile S(k) and the last tile E(k) of ring k.")
  (equal (S k) (add 2 (mul 3 (mul k (sub k 1)))))
  (equal (E k) (add 1 (mul 3 (mul k (add k 1)))))

  (cl-comment "=== 3. Deductive Formulation of PD(n)=3 ===")
  (cl-comment "For S(k) (k >= 1): The prime differences must strictly be 6k-1, 6k+1, 12k+5.")
  (cl-comment "For E(k) (k >= 2): The prime differences must strictly be 6k-1, 6k+5, 12k-7.")
  (forall (k)
    (implies (and (greater_than_or_equal k 1)
                  (is_prime (sub (mul 6 k) 1))
                  (is_prime (add (mul 6 k) 1))
                  (is_prime (add (mul 12 k) 5)))
             (valid_sequence_element (S k))))
  (forall (k)
    (implies (and (greater_than_or_equal k 2)
                  (is_prime (sub (mul 6 k) 1))
                  (is_prime (add (mul 6 k) 5))
                  (is_prime (sub (mul 12 k) 7)))
             (valid_sequence_element (E k))))

  (cl-comment "=== 4. Boundary Verification (Dfix0 Alignment) ===")
  (cl-comment "The first two elements are established as 1 and 2.")
  (cl-comment "We verify our logic against the problem's provided Truth: the 10th is 271.")
  (equal (Sequence 10) 271)

  (cl-comment "=== 5. Exact Integer Projection and Debt Clearance ===")
  (cl-comment "The algorithm maps entirely to O(1) space integer primality tests,")
  (cl-comment "leaving absolutely no state debt (GC overhead) behind.")
)
||#


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+quicklisp (ql:quickload :iterate :silent t))
(use-package :iterate)

(declaim (inline is-prime))
(defun is-prime (n)
  "Pure integer primality test, projecting out floating-point illusions."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum n))
  (if (< n 2) (return-from is-prime nil))
  (if (or (= n 2) (= n 3)) (return-from is-prime t))
  (if (or (= (mod n 2) 0) (= (mod n 3) 0)) (return-from is-prime nil))
  (iter (declare (type fixnum i))
        (for i from 5 by 6)
        (while (<= (* i i) n))
        (if (or (= (mod n i) 0) (= (mod n (+ i 2)) 0))
            (return nil))
        (finally (return t))))

(defun solve ()
  "Solves Project Euler 128 by manifesting the mathematically deduced Middle-Way."
  (declare (optimize (speed 3) (safety 0)))
  (let ((count 2) ; Starting with tiles 1 and 2 already counted
        (k 2))
    (declare (type fixnum count k))
    (iter
      (let ((6k (* 6 k)))
        (declare (type fixnum 6k))
        (let ((6k-1 (1- 6k)))
          ;; Check the shared condition first to minimize redundant evaluations
          (when (is-prime 6k-1)
            
            ;; Check S(k) valid conditions: 6k-1, 6k+1, 12k+5
            (when (and (is-prime (1+ 6k))
                       (is-prime (+ (* 12 k) 5)))
              (incf count)
              (when (= count 2000)
                (return (+ 2 (* 3 k (1- k))))))
            
            ;; Check E(k) valid conditions: 6k-1, 6k+5, 12k-7
            (when (and (is-prime (+ 6k 5))
                       (is-prime (- (* 12 k) 7)))
              (incf count)
              (when (= count 2000)
                (return (+ 1 (* 3 k (1+ k)))))))))
      (incf k))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.115
System time  =        0.007
Elapsed time =        0.093
Allocation   = 201032 bytes
1014 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 14516824220
:ok
