;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0294 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0294)

#||
(cl-comment "SKDT – Dual Sunyata Structures and Emergent Category Theory")
(cl-comment "Title   : Emergence of S(n) via Matrix Doubling (Two-Truths Slice Category)")
(cl-comment "Author  : Masaomi Chiba")
(cl-comment "Date    : 2025-12-01")

[Analysis: The Middle Way of Computation]
1.  **Potential (Kusokuzeshiki - K):** The space of all digit strings of length N. The combinatorial explosion O(10^N) makes direct enumeration impossible (Sunyata).
2.  **Manifest (Shikisokuzeku - S):** The specific subset of strings satisfying d(k)=23 and k ≡ 0 (mod 23).
3.  **The Adjunction (F ⊣ G):** We define a functor that maps a block of length L to its distribution of (digit_sum, remainder_mod_23). The "doubling" operation (Matrix Exponentiation) allows us to jump from length L to 2L in O(S^2 * R^2) time, where S=24 and R=23.
4.  **The Fixed Point (Dfix0):** The final distribution at length N = 11^12.
5.  **Complexity:** O(log N * 24^2 * 23^2). For N ≈ 3*10^12, log2(N) ≈ 42. Total operations ≈ 42 * 300 * 529 ≈ 6.6 * 10^6, well within the 1-minute (10^7-10^8) redline.

[Mathematical Shortcut]
- Let V(L) be a 24x23 matrix where V(L)[s][r] is the count of strings of length L with digit sum 's' and value ≡ 'r' (mod 23).
- V(L1 + L2)[s1 + s2][(r1 * 10^L2 + r2) mod 23] = Σ V(L1)[s1][r1] * V(L2)[s2][r2].
- Binary exponentiation (Doubling) computes V(11^12) efficiently.
||#

(declaim (inline combine))
(defun combine (v1 v2 p)
  "Combines two distributions v1 (high part) and v2 (low part).
   p = 10^(length of v2) mod 23.
   Complexity: O(S^2 * R^2) where S=24, R=23."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 64) (24 23)) v1 v2)
           (type (integer 0 22) p))
  (let ((res (make-array '(24 23) :element-type '(unsigned-byte 64) :initial-element 0))
        (mod-val #.(expt 10 9)))
    (declare (type (simple-array (unsigned-byte 64) (24 23)) res)
             (type (unsigned-byte 64) mod-val))
    (iterate (for s1 from 0 to 23)
             (iterate (for r1 from 0 below 23)
                      (for val1 = (aref v1 s1 r1))
                      (unless (zerop val1)
                        (let ((r1-p (mod (* r1 p) 23)))
                          (declare (type (integer 0 22) r1-p))
                          (iterate (for s2 from 0 to (- 23 s1))
                                   (for s-sum = (+ s1 s2))
                                   (iterate (for r2 from 0 below 23)
                                            (let ((r-sum (+ r1-p r2)))
                                              (when (>= r-sum 23) (decf r-sum 23))
                                              (setf (aref res s-sum r-sum)
                                                    (mod (+ (aref res s-sum r-sum)
                                                            (* val1 (aref v2 s2 r2)))
                                                         mod-val)))))))))
    res))

(defun solve (&optional (n (expt 11 12)))
  "Solves Project Euler 294 for N = 11^12 mod 10^9."
  (format t "Initializing Binary Exponentiation for S(N) where N = ~A...~%" n)
  (let ((v1 (make-array '(24 23) :element-type '(unsigned-byte 64) :initial-element 0))
        (res-v (make-array '(24 23) :element-type '(unsigned-byte 64) :initial-element 0))
        (base-p 10)
        (exp n))
    ;; Identity state (length 0): sum=0, rem=0
    (setf (aref res-v 0 0) 1)
    
    ;; Base state (length 1): digits 0-9
    (iterate (for d from 0 to 9)
             (setf (aref v1 d (mod d 23)) 1))
    
    (let ((base-v v1)
          (start-time (get-internal-real-time)))
      (iterate (while (> exp 0))
               (when (oddp exp)
                 (setf res-v (combine res-v base-v base-p)))
               (setf base-v (combine base-v base-v base-p))
               (setf base-p (mod (* base-p base-p) 23))
               (setf exp (ash exp -1)))
      
      (let ((ans (aref res-v 23 0))
            (end-time (get-internal-real-time)))
        (format t "Computation complete in ~,3F seconds.~%" 
                (/ (- end-time start-time) internal-time-units-per-second))
        (format t "Result S(~A) mod 10^9: ~A~%" n ans)
        ans))))

;;; --- Execution ---
;; (project-euler-0294:solve)

#||
[Self-Analysis]
1.  **Constraint Utilization:** 
    The problem asks for d(k)=23. This limits the sum of digits to a very small range (0-23), 
    effectively collapsing the state space from O(10^N) to O(24 * 23).
    The condition k ≡ 0 (mod 23) further constrains the state.

2.  **Termination and Performance:**
    The code uses binary exponentiation (doubling). The number of iterations is exactly 
    ceil(log2(11^12)) ≈ 42. Each iteration performs ≈ 158,700 inner loop operations. 
    Total complexity is O(log N). This will finish in a few milliseconds on any modern 
    Common Lisp implementation (SBCL, CCL). There is no possibility of an infinite loop 
    as the exponent 'exp' is halved in every step.

3.  **LLM Traps:**
    - **Trap 1: Combinatorial explosion.** Trying to use digit-sum partitions or 
      generating functions without the mod 23 constraint.
    - **Trap 2: Leading zeros.** S(n) counts k < 10^n. My DP counts strings of length N 
      including leading zeros. Since d(0)=0 and we need d(k)=23, k=0 is never counted, 
      so the string count perfectly matches the integer count for positive k.
    - **Trap 3: Modulo Arithmetic.** Intermediate products (10^9 * 10^9) require 
      64-bit integers. Common Lisp handles this naturally, but one must ensure 
      (unsigned-byte 64) is used to avoid unnecessary bignum allocation or overflow.
||#


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing Binary Exponentiation for S(N) where N = 3138428376721...
Computation complete in 0.195 seconds.
Result S(3138428376721) mod 10^9: 789184709

User time    =        0.238
System time  =        0.014
Elapsed time =        0.195
Allocation   = 664016 bytes
3688 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 789184709
:ok