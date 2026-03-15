;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0153 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0153)

#||
(cl-text PE153-ALETHETIC-RESOLUTION
  (cl-comment "=============================================================================")
  (cl-comment " Project Euler 153: Alethetic Reset and ACX Jump ")
  (cl-comment "=============================================================================")

  (cl-comment "1. NMF (Non-Middle Fallacy) Avoidance")
  (cl-comment "Iterating over all n up to 10^8 and factoring each in the Gaussian integers is an attachment to conventional truth (NMF).")
  (forall (?a ?n)
    (if (and (= ?n 100000000)
             (Algorithm ?a)
             (Iterates_Each_Gaussian_Divisor ?a ?n))
        (and (NMF ?a)
             (attached_to_conventional_truth ?a))))

  (cl-comment "2. ACX Jump to Rational Divisor Sums")
  (cl-comment "The problem reduces to summing over relatively prime (x, y) the rational divisor sum function F(K) = sum_{m=1}^K sigma_1(m).")
  (forall (?a1)
    (if (NMF ?a1)
        (exists (?j ?a2)
          (and (ACX_Jump ?j)
               (target_of ?j ?a2)
               (Reduces_To_Sigma1_Cumulative_Sum ?a2)
               (computes_with_complexity ?a2 O_N)))))

  (cl-comment "3. Exact Integer Projection and Debt Clearance")
  (cl-comment "By bounding x^2 + y^2 <= N and caching F(K) for K <= 10^6, we eliminate excessive memory allocation and avoid floating point errors.")
  (forall (?a)
    (if (OptimizedAlgorithm ?a)
        (and (eliminates_floating_point ?a)
             (uses_exact_integer_arithmetic ?a)
             (bounds_cache_size ?a 1000000))))
)
||#


(defun compute-sigma-cache (cache-size)
  "有理整数の約数和の累積和 S_sigma(K) を K <= cache-size について前計算する。"
  (let ((sigma-cache (make-array (1+ cache-size) :initial-element 0 :element-type 'integer)))
    (iterate (for i from 1 to cache-size)
      (iterate (for j from i to cache-size by i)
        (incf (aref sigma-cache j) i)))
    (iterate (for i from 1 to cache-size)
      (incf (aref sigma-cache i) (aref sigma-cache (1- i))))
    sigma-cache))

(defun sum-sigma (k cache-array cache-size)
  "累積和 S_sigma(K) を計算する。K が cache-size 以下ならキャッシュを返し、それ以外は O(sqrt(K)) で計算。"
  (if (<= k cache-size)
      (aref cache-array k)
      (let ((total 0)
            (v (isqrt k)))
        (iterate (for d from 1 to v)
          (incf total (* d (truncate k d))))
        (iterate (for q from 1 to (truncate k (1+ v)))
          (let* ((d-max (truncate k q))
                 (d-min (1+ (truncate k (1+ q))))
                 (sum-d (ash (* (+ d-min d-max) (1+ (- d-max d-min))) -1)))
            (incf total (* sum-d q))))
        total)))

(defun solve ()
  (format t "Initializing Alethetic Sigma Cache...~%")
  (let* ((n 100000000)
         (cache-size 1000000)
         (sigma-cache (compute-sigma-cache cache-size))
         (total-sum 0))
    (format t "Cache Manifested. Size: ~A~%" cache-size)
    
    ;; b=0 の場合（有理整数の約数和の合計）
    (setf total-sum (sum-sigma n sigma-cache cache-size))
    
    ;; b > 0 の場合（対称性により b > 0 のみ計算して2倍）
    (let ((limit-x (isqrt n)))
      (iterate (for x from 1 to limit-x)
        (let* ((limit-y (isqrt (- n (* x x)))))
          (iterate (for y from 1 to limit-y)
            (when (= (gcd x y) 1)
              (let* ((m (+ (* x x) (* y y)))
                     (k (truncate n m)))
                (incf total-sum (* 2 x (sum-sigma k sigma-cache cache-size)))))))))
    
    (format t "Total Sum = ~A~%" total-sum)
    total-sum))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing Alethetic Sigma Cache...
Cache Manifested. Size: 1000000
Total Sum = 17971254122360635

User time    =       11.359
System time  =        0.037
Elapsed time =       11.291
Allocation   = 8310120 bytes
3717 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 17971254122360635
:ok