;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0241 (:use cl #|alexandria|# iterate))
(in-package #:project-euler-0241)

#||
(cl-text euler-acx-p241

  (cl-comment "Ontology for Euler-ACX P241: Perfection Quotients")
  (cl-comment "Based on Two-Truths Entanglement (二諦随伴) and SKDT")

  (cl-comment "=== 1. Problem Analysis & NMF Avoidance ===")
  (cl-comment "Finding n <= 10^18 such that p(n) = k + 1/2.")
  (cl-comment "N=10^18 forces us to reject O(N) traversal (NMF). We must jump (ACX) to Ultimate Truth.")
  (forall (n k)
    (if (and (<= n 1000000000000000000)
             (= (/ (sigma n) n) (+ k 1/2)))
        (exists (a u c)
          (and (= n (* (expt 2 a) u))
               (odd u)
               (>= a 1)
               (= c (+ (* 2 k) 1))
               (= (/ (sigma u) u) (/ (* c (expt 2 (- a 1))) (- (expt 2 (+ a 1)) 1)))
               (= (v2-of (sigma u)) (- a 1))))))

  (cl-comment "=== 2. Exact Integer Projection ===")
  (cl-comment "We eliminate floats by manipulating the exact target fraction F = N_target / D_target.")
  (cl-comment "At any step, remaining required ratio R = R_num / R_den must be reached via odd primes.")
  (forall (R_num R_den u_rem)
    (if (and (= (/ (sigma u_rem) u_rem) (/ R_num R_den))
             (= (gcd R_num R_den) 1))
        (and (divides R_den u_rem)
             (odd R_den))))

  (cl-comment "=== 3. Bijective Generation & Pruning ===")
  (cl-comment "If R_den is even, no odd primes can satisfy the division, so we prune.")
  (cl-comment "If the minimal prime factor of R_den is smaller than the current prime index, we prune.")
  (forall (R_den p_idx)
    (if (or (even R_den)
            (< (min_prime_factor R_den) p_idx))
        (not (reachable R_den p_idx))))

)
||#

(defvar *ans-sum* 0)

(defun dfloat (n)
  (float n 0d0))

(defun v2-of (n)
  (if (zerop n)
      0
      (1- (integer-length (logand n (- n))))))

(defun sieve (limit)
  "Generate an array of odd primes up to limit."
  (let ((s (make-array (1+ limit) :element-type 'bit :initial-element 0))
        (primes (make-array 100000 :fill-pointer 0 :adjustable t)))
    (iterate
      (for i from 3 to limit by 2)
      (when (zerop (sbit s i))
        (vector-push-extend i primes)
        (iterate
          (for j from (* i i) to limit by (* 2 i))
          (setf (sbit s j) 1))))
    primes))

(defun min-prime-factor (n primes)
  "Find the smallest prime factor of n using the primes array."
  (if (= n 1)
      nil
      (iterate
        (for p in-vector primes)
        (when (> (* p p) n) (leave n))
        (when (zerop (mod n p)) (leave p))
        (finally (return n)))))

(defun max-ratio-from (p-idx max-prod primes)
  "Calculate an upper bound for the ratio product achievable using primes starting from p-idx."
  (let ((ratio 1.0d0)
        (prod 1))
    (iterate
      (for i from p-idx below (length primes))
      (for q = (aref primes i))
      (if (<= (* prod q) max-prod)
          (progn
            (setf prod (* prod q))
            (setf ratio (* ratio (/ (dfloat q) (- (dfloat q) 1.0d0)))))
          (progn
            ;; Add one fractional step to ensure the bound is strictly an overestimate
            (setf ratio (* ratio (/ (dfloat q) (- (dfloat q) 1.0d0))))
            (leave))))
    ratio))

(defun power-mod (base exp m)
  (let ((res 1))
    (setf base (mod base m))
    (iterate
      (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (* res base) m)))
      (setf base (mod (* base base) m))
      (setf exp (ash exp -1)))
    res))

(defun miller-rabin (n &optional (k 5))
  "Miller-Rabin primality test for large primes."
  (cond ((< n 2) (return-from miller-rabin nil))
        ((or (= n 2) (= n 3)) (return-from miller-rabin t))
        ((evenp n) (return-from miller-rabin nil)))
  (let ((d (1- n))
        (s 0))
    (iterate
      (while (evenp d))
      (setf d (ash d -1))
      (incf s))
    (iterate
      (repeat k)
      (for a = (+ 2 (random (- n 3))))
      (for x = (power-mod a d n))
      (unless (or (= x 1) (= x (1- n)))
        (iterate
          (for r from 1 below s)
          (setf x (power-mod x 2 n))
          (when (= x 1) (return-from miller-rabin nil))
          (when (= x (1- n)) (leave)))
        (unless (= x (1- n))
          (return-from miller-rabin nil))))
    t))

(defun dfs (p-idx u sigma v2-rem limit-u a N-target D-target primes)
  (let* ((num (* N-target u))
         (den (* D-target sigma))
         (g (gcd num den))
         (R-num (/ num g))
         (R-den (/ den g)))
    
    ;; 1. Goal Check
    (when (and (= R-num 1) (= R-den 1))
      (when (= v2-rem 0)
        (incf *ans-sum* (* u (ash 1 a))))
      (return-from dfs))

    ;; 2. Exact Integer Projection Pruning
    (when (< R-num R-den) (return-from dfs))
    (when (< v2-rem 0) (return-from dfs))
    (when (evenp R-den) (return-from dfs))

    ;; 3. Bijective Generation Pruning
    (let ((min-p (min-prime-factor R-den primes)))
      (when (and min-p (< min-p (aref primes p-idx)))
        (return-from dfs)))

    ;; 4. Check for single large prime > max array prime
    (let ((diff (- R-num R-den)))
      (when (and (> diff 0) (zerop (mod R-den diff)))
        (let ((p-large (/ R-den diff)))
          (when (and (> p-large (aref primes (1- (length primes))))
                     (<= (* u p-large) limit-u)
                     (miller-rabin p-large))
            (let ((v2 (v2-of (1+ p-large))))
              (when (= v2 v2-rem)
                (incf *ans-sum* (* u p-large (ash 1 a)))))))))

    ;; 5. Iterate allowed primes dynamically
    (let ((float-R (/ (dfloat R-num) R-den)))
      (iterate
        (for i from p-idx below (length primes))
        (for p = (aref primes i))
        (when (> (* u p) limit-u) (leave))
        
        ;; Dynamic bounding constraint
        (for max-rat = (max-ratio-from i (floor limit-u u) primes))
        (when (< (+ max-rat 1d-9) float-R)
          (leave))

        (let ((pe p)
              (sigma-pe (1+ p)))
          (iterate
            (while (<= (* u pe) limit-u))
            (let ((v2 (v2-of sigma-pe)))
              (when (<= v2 v2-rem)
                (dfs (1+ i) (* u pe) (* sigma sigma-pe) (- v2-rem v2)
                     limit-u a N-target D-target primes)))
            (setf pe (* pe p))
            (setf sigma-pe (+ sigma-pe pe))))))))

(defun solve-241 ()
  (setf *ans-sum* 0)
  (let* ((limit 10000000) ; We use 10^7 as the base threshold for explicit iteration
         (primes (sieve limit)))
    (iterate
      (for a from 1 to 59)
      (for limit-u = (floor 1000000000000000000 (ash 1 a)))
      (when (< limit-u 1) (leave))
      (for D-target = (1- (ash 1 (1+ a))))
      (iterate
        (for c in '(3 5 7 9 11 13 15 17 19))
        (for N-target = (* c (ash 1 (1- a))))
        (dfs 0 1 1 (- a 1) limit-u a N-target D-target primes))))
  *ans-sum*)


#+| Do it | (solve-241 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-241)

User time    =       16.019
System time  =        0.058
Elapsed time =       16.113
Allocation   = 2082696048 bytes
2552 Page faults
GC time      =        0.021
 |------------------------------------------------------------|#
;;→ 482316491800641154
:ok
