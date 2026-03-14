;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0223 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0223)

#||
(cl-text EULER-ACX-DIFD-INTEGRATION
  (cl-comment "
  =============================================================================
  ARX-Core: Structural Gravity Protocol for PE 0223 (Discrete DIFD)
  =============================================================================
  Formalization of the Alethetic Reset: Transitioning from O(N^2) geometric 
  search to Orthogonal Projection via Bounded Factorization with Parity Invariance.
  ")

  (cl-comment "1. NMF (Non-Middle Fallacy) Detection")
  (forall (?solver)
    (if (and (Solves ?solver PE0223)
             (Explores ?solver (CartesianProduct A B)))
        (and (NMF ?solver)
             (ProducesHallucination (CombinatorialExplosion ?solver))
             (ExceedsTimeLimit 60))))

  (cl-comment "2. ACX Jump: Orthogonal Projection via Factorization")
  (cl-comment "The equation a^2 + b^2 = c^2 + 1 reformulates to (c-b)(c+b) = a^2 - 1.
               Let u = c-b and v = c+b.
               The geometry strictness a <= b <= c imposes boundaries on u:
               b >= a  ==>  u <= sqrt(2a^2 - 1) - a
               a+b+c <= L  ==>  u >= (a^2 - 1) / (L - a).
               This constrains the search space for u into a microscopic window.")
  (forall (?a ?b ?c ?u)
    (iff (ValidBarelyAcute ?a ?b ?c)
         (and (Divides ?u (- (^ ?a 2) 1))
              (<= (Ceil (/ (- (^ ?a 2) 1) (- L ?a))) ?u)
              (<= ?u (- (Sqrt (- (* 2 (^ ?a 2)) 1)) ?a))
              (Equal (mod ?u 2) (mod (/ (- (^ ?a 2) 1) ?u) 2)))))

  (cl-comment "3. Middle Way Manifestation (Parity Elimination)")
  (cl-comment "If 'a' is even, a^2-1 is odd, and all divisors automatically match parity.
               If 'a' is odd, a^2-1 is a multiple of 8. By strictly bounding the 
               exponent of prime 2 to [1, k_2 - 1], the divisor u and cofactor v 
               are forced to both be even. This eliminates modulo checks entirely 
               at the leaf nodes of the search tree.")
)
||#

(defun build-sieve (size)
  "Builds a Smallest Prime Factor (SPF) sieve up to the specified size."
  (let ((min-prime (make-array size :element-type 'fixnum :initial-element 0)))
    (declare (type (simple-array fixnum (*)) min-prime)
             (optimize (speed 3) (safety 0) (debug 0)))
    (iterate (for i from 2 below size)
      (setf (aref min-prime i) i))
    (iterate (for i from 2 below (isqrt size))
      (when (= (aref min-prime i) i)
        (iterate (for j from (* i i) below size by i)
          (when (= (aref min-prime j) j)
            (setf (aref min-prime j) i)))))
    min-prime))

(defun solve ()
  "Solves PE 223 by factoring a^2-1 and counting valid divisors in the geometric window."
  (let* ((L 25000000)
         ;; Calculate the max possible 'a' based on the intersection of bounds
         ;; (a^2 - 1)/(L - a) <= sqrt(2a^2 - 1) - a  ==>  a <= ~7322330
         (A-MAX 7322334)
         (sieve (build-sieve (1+ A-MAX)))
         ;; Handle a=1 case separately: 1^2 + b^2 = c^2 + 1 => b = c.
         ;; Perimeter constraint: 1 + 2b <= L => b <= (L-1)/2
         (ans (floor (1- L) 2))
         
         ;; Flat arrays to store prime factorization data for DFS
         (p-arr (make-array 20 :element-type 'fixnum))
         (min-e-arr (make-array 20 :element-type 'fixnum))
         (max-e-arr (make-array 20 :element-type 'fixnum))
         ;; 1D flattened array for powers (20 x 64) to maximize cache locality
         (powers-arr (make-array 1280 :element-type 'fixnum :initial-element 0))
         (max-mult-arr (make-array 21 :element-type 'fixnum))
         (min-u 0)
         (max-u 0)
         (num-factors 0))
    (declare (type fixnum L A-MAX ans min-u max-u num-factors)
             (type (simple-array fixnum (*)) sieve p-arr min-e-arr max-e-arr max-mult-arr powers-arr)
             (optimize (speed 3) (safety 0) (debug 0)))
    
    (labels ((dfs (idx current-u)
               "Finds valid divisors within the [min-u, max-u] window via Branch and Bound."
               (declare (type fixnum idx current-u))
               ;; Prune if the partial divisor exceeds the maximum possible bound
               (if (> current-u max-u)
                   0
                   ;; Prune if even maximizing all remaining primes falls short of minimum
                   (if (< (* current-u (aref max-mult-arr idx)) min-u)
                       0
                       ;; Valid leaf
                       (if (= idx num-factors)
                           1
                           (let ((sum 0)
                                 (mine (aref min-e-arr idx))
                                 (maxe (aref max-e-arr idx)))
                             (declare (type fixnum sum mine maxe))
                             (iterate (for e from mine to maxe)
                               ;; Fetch precomputed prime power: p^e
                               (incf sum (dfs (1+ idx) 
                                              (* current-u (aref powers-arr (+ (ash idx 6) e))))))
                             sum))))))
      
      (iterate (for a from 2 to A-MAX)
        (let* ((a2 (* a a))
               (N-val (1- a2))
               (D-val (- L a)))
          (declare (type fixnum a2 N-val D-val))
          
          ;; Compute u bounds based on geometric constraints
          ;; min-u = ceil((a^2 - 1) / (L - a))
          (setf min-u (floor (+ N-val D-val -1) D-val))
          ;; max-u = floor(sqrt(2a^2 - 1) - a)
          (setf max-u (- (isqrt (1- (* 2 a2))) a))
          
          ;; Only proceed if the window is mathematically open
          (when (<= min-u max-u)
            (setf num-factors 0)
            (let ((count-2 0))
              (declare (type fixnum count-2))
              
              ;; 1. Factorize (a - 1) using the precomputed sieve in O(log a)
              (let ((n (1- a)))
                (declare (type fixnum n))
                (iterate (while (> n 1))
                  (let ((p (aref sieve n))
                        (count 0))
                    (declare (type fixnum p count))
                    (iterate (while (= (aref sieve n) p))
                      (incf count)
                      (setf n (truncate n p)))
                    (if (= p 2)
                        (incf count-2 count)
                        (progn
                          (setf (aref p-arr num-factors) p)
                          (setf (aref min-e-arr num-factors) 0)
                          (setf (aref max-e-arr num-factors) count)
                          (incf num-factors))))))
              
              ;; 2. Factorize (a + 1) using the sieve
              ;; Note: (a-1) and (a+1) differ by 2, so their only shared prime factor is 2.
              ;; Thus, all other primes are strictly disjoint and require no complex merging.
              (let ((n (1+ a)))
                (declare (type fixnum n))
                (iterate (while (> n 1))
                  (let ((p (aref sieve n))
                        (count 0))
                    (declare (type fixnum p count))
                    (iterate (while (= (aref sieve n) p))
                      (incf count)
                      (setf n (truncate n p)))
                    (if (= p 2)
                        (incf count-2 count)
                        (progn
                          (setf (aref p-arr num-factors) p)
                          (setf (aref min-e-arr num-factors) 0)
                          (setf (aref max-e-arr num-factors) count)
                          (incf num-factors))))))
              
              ;; 3. Handle the Parity Restrictor for Prime 2
              (when (> count-2 0)
                (setf (aref p-arr num-factors) 2)
                (if (oddp a)
                    ;; If 'a' is odd, a^2-1 is even. Both u and v must be even.
                    ;; We force the exponent of 2 in u to strictly be between 1 and total_count-1.
                    (progn
                      (setf (aref min-e-arr num-factors) 1)
                      (setf (aref max-e-arr num-factors) (1- count-2)))
                    ;; If 'a' is even, a^2-1 is odd (handled naturally, count-2 would be 0, 
                    ;; but logically it would just be 0 to 0)
                    (progn
                      (setf (aref min-e-arr num-factors) 0)
                      (setf (aref max-e-arr num-factors) count-2)))
                (incf num-factors))
              
              ;; 4. Setup Power Arrays and Max-Multiplier Bound Limits
              (setf (aref max-mult-arr num-factors) 1)
              (iterate (for i from (1- num-factors) downto 0)
                (let ((p (aref p-arr i))
                      (maxe (aref max-e-arr i))
                      (pow 1))
                  (declare (type fixnum p maxe pow))
                  (iterate (for e from 0 to maxe)
                    (setf (aref powers-arr (+ (ash i 6) e)) pow)
                    (when (< e maxe)
                      (setf pow (* pow p))))
                  ;; max-mult bounds the maximum possible multiplier suffix
                  (setf (aref max-mult-arr i) 
                        (* (aref max-mult-arr (1+ i)) pow))))
              
              ;; 5. Explore the narrowed tree for this specific 'a'
              (incf ans (dfs 0 1)))))))
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =       15.277
System time  =        0.112
Elapsed time =       15.308
Allocation   = 58910704 bytes
17127 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 61614848
:ok
