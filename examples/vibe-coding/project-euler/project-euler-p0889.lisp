;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0889 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0889)

#||
(cl:comment "PE 889 Mathematical Constraints and Shortcuts")
(cl:comment "Invariant 1: The input argument to the Takagi (blancmange) function is x = A(2^k - 1) / (2^{2k} - 1) where A = (2^t + 1)^r. This implies x has a purely periodic binary expansion of period 2k.")
(cl:comment "Invariant 2: For A < 2^k, the 2k-bit period of x is exactly the bitwise anti-periodic string formed by A' = A - 1 in the upper k bits, and its bitwise NOT in the lower k bits.")
(cl:comment "Constraint 1: The formula for F(k, t, r) can be completely algebraically expanded and reduced from a sum of O(k) terms down to a sum over only the non-zero bits of A'.")
(cl:comment "Constraint 2: Because t = 10^14 + 31 is vastly larger than the bit-length of C(r, i) for r=62, the binary representation of A' = sum_{i=1}^r C(r, i) 2^{t*i} has NO overlapping blocks. It is extremely sparse.")
(cl:comment "Shortcut: We map the exact function to F = k(A'+1)2^k + N(2A' - 2^{k+1}) - (2^k + 1) * sum_{j: bit j is 1} 2^j (j + 2 C_j), where N is the popcount of A' and C_j is the number of 1s strictly above bit j. This reduces O(10^{18}) iterations to precisely O(r * log(C(r, r/2))) ~ 4000 iterations.")
||#

(defun power-mod (base exp m)
  "Calculates (base^exp) mod m efficiently."
  (let ((res 1))
    (iterate (while (> exp 0))
      (for b initially (mod base m) then (mod (* b b) m))
      (when (oddp exp)
        (setf res (mod (* res b) m)))
      (setf exp (ash exp -1)))
    res))

(defun solve ()
  (let* ((modulo 1000062031)
         (k (+ (expt 10 18) 31))
         (t-shift (+ (expt 10 14) 31))
         (r 62)
         (a-prime-mod 0)
         (total-popcount 0)
         (sparse-sum 0)
         (c-array (make-array (1+ r) :element-type 'integer :initial-element 0))
         (pop-array (make-array (1+ r) :element-type 'fixnum :initial-element 0)))
    
    (format t "Precomputing non-overlapping binomial blocks for A'...~%")
    ;; Precompute binomial coefficients C(r, i) and their popcounts
    ;; A' = A - 1 = sum_{i=1}^r C(r, i) 2^{t*i}
    (setf (aref c-array 0) 1)
    (iterate (for i from 1 to r)
      (setf (aref c-array i) (/ (* (aref c-array (1- i)) (1+ (- r i))) i))
      (setf (aref pop-array i) (logcount (aref c-array i)))
      (incf total-popcount (aref pop-array i)))
      
    (format t "Aggregating sparse bit components in O(r * log(C(r, r/2)))...~%")
    (iterate (for i from 1 to r)
      (let* ((c-i (aref c-array i))
             (block-shift (* t-shift i))
             (term-val (power-mod 2 block-shift modulo)))
        
        ;; Accumulate A' mod M
        (setf a-prime-mod (mod (+ a-prime-mod (mod (* (mod c-i modulo) term-val) modulo)) modulo))
        
        ;; Iterate over the bits of the binomial coefficient
        (iterate (for u from 0 to 62)
          (when (logbitp u c-i)
            (let* ((j-val (+ block-shift u))
                   (j-mod (mod j-val modulo))
                   ;; C_j: Number of 1s in A' strictly above the current bit j
                   (c-j (+ (logcount (ash c-i (- (1+ u))))
                           (iterate (for i-prime from (1+ i) to r)
                             (sum (aref pop-array i-prime)))))
                   (two-to-j (power-mod 2 j-val modulo))
                   ;; Add 2^j * (j + 2 * C_j) to the sparse sum
                   (term (mod (* two-to-j (mod (+ j-mod (* 2 c-j)) modulo)) modulo)))
              (setf sparse-sum (mod (+ sparse-sum term) modulo)))))))
              
    (format t "Evaluating the collapsed closed-form formula...~%")
    (let* ((k-mod (mod k modulo))
           (two-to-k (power-mod 2 k modulo))
           (two-to-k-plus-1 (mod (* 2 two-to-k) modulo))
           
           ;; Term 1: k * (A' + 1) * 2^k
           (term1 (mod (* k-mod (mod (* (1+ a-prime-mod) two-to-k) modulo)) modulo))
           
           ;; Term 2: N * (2A' - 2^{k+1})
           (term2 (mod (* total-popcount (mod (- (* 2 a-prime-mod) two-to-k-plus-1) modulo)) modulo))
           
           ;; Term 3: (2^k + 1) * sparse-sum
           (term3 (mod (* (1+ two-to-k) sparse-sum) modulo))
           
           ;; Final Assembly: F = Term1 + Term2 - Term3
           (ans (mod (- (+ term1 term2) term3) modulo)))
           
      (format t "Final Answer F(~A, ~A, ~A) mod ~A: ~A~%" k t-shift r modulo ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputing non-overlapping binomial blocks for A'...
Aggregating sparse bit components in O(r * log(C(r, r/2)))...
Evaluating the collapsed closed-form formula...
Final Answer F(1000000000000000031, 100000000000031, 62) mod 1000062031: 424315113

User time    =        0.006
System time  =        0.000
Elapsed time =        0.004
Allocation   = 119960 bytes
24 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 424315113
:ok