;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0969 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0969)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

;;; ===========================================================================
;;; Problem 969: Expected Kangaroo Hops
;;; ===========================================================================
;;; The expected number of hops to pass distance x is H(x) = sum_{k=0}^floor(x) [(-1)^k (x-k)^k / k!] e^(x-k).
;;; Let alpha = H(1) = e. Then H(n) is a polynomial in alpha:
;;; H(n) = sum_{k=0}^{n-1} [(-1)^k (n-k)^k / k!] alpha^(n-k).
;;; S(n) is the sum of integer coefficients of this polynomial.
;;; A coefficient c_j for alpha^j is an integer if (n-j)! divides j^(n-j).
;;; Let m = n-j. The condition is m! divides (n-m)^m.
;;; This is equivalent to L_m | (n-m), where L_m is the primorial of m.
;;; The total sum over n=1 to N is sum_{k=0}^{N-1} [(-1)^k L_k^k / k!] * sum_{j=1}^{K_k} j^k,
;;; where K_k = floor((N-k)/L_k).
;;; We compute this modulo 10^9 + 7.
;;; ===========================================================================

(defparameter *modulus* (+ (expt 10 9) 7))

(defun mod-inv (n)
  (mod-expt n (- *modulus* 2) *modulus*))

(defun make-fixnum-array (size &key (initial-element 0))
  (make-array size :element-type 'fixnum :initial-element initial-element))


(defun mod-expt (base power modulus)
  "Computes (base^power) modulo modulus using binary exponentiation."
  (let ((result 1)
        (b (mod base modulus))
        (p power))
    (loop while (plusp p) do
      (when (oddp p)
        (setf result (mod (* result b) modulus)))
      (setf b (mod (* b b) modulus))
      (setf p (ash p -1)))
    result))

(defun compute-bernoulli (limit)
  "Computes Bernoulli numbers B_j+ up to limit."
  (let ((bern (make-array (1+ limit) :initial-element 0))
        (comb (make-array (list (+ limit 2) (+ limit 2)) :element-type 'fixnum :initial-element 0)))
    
    ;; 修正: 二次元配列 (aref comb n k) として正しくアクセスし Pascalの三角形を構築
    (iterate ((n (scan-range :from 0 :upto (1+ limit))))
      (setf (aref comb n 0) 1)
      (iterate ((k (scan-range :from 1 :upto n)))
        (setf (aref comb n k) 
              (mod (+ (aref comb (1- n) (1- k))
                      (aref comb (1- n) k))
                   *modulus*))))
    
    ;; Recurrence for B_j-
    (setf (aref bern 0) 1)
    (iterate ((m (scan-range :from 1 :upto limit)))
      (let ((sum 0))
        ;; 修正: ベルヌーイ数の計算部分の二次元配列アクセスも修正
        (iterate ((j (scan-range :from 0 :upto (1- m))))
          (setf sum (mod (+ sum (* (aref comb (1+ m) j) (aref bern j))) *modulus*)))
        (setf (aref bern m) 
              (mod (* (mod (- sum) *modulus*) 
                      (mod-inv (aref comb (1+ m) m))) 
                   *modulus*))))
    
    ;; Convert B_1- to B_1+
    (when (>= limit 1)
      (setf (aref bern 1) (mod-inv 2)))
    bern))

(defun faulhaber-sum (n k bernoulli-numbers)
  "Computes sum_{j=1}^n j^k modulo *modulus* using Faulhaber's formula."
  (if (= n 0) 0
      (let* ((n-mod (mod n *modulus*))
             (k+1 (1+ k))
             (sum 0)
             (comb-row (make-fixnum-array (+ k+1 1))))
        ;; We need C(k+1, j) for j=0..k
        (setf (aref comb-row 0) 1)
        (iterate ((j (scan-range :from 1 :upto k)))
          (setf (aref comb-row j) (mod (* (aref comb-row (1- j)) (mod-inv j) (- k+1 (1- j))) *modulus*)))
        
        (iterate ((j (scan-range :from 0 :upto k)))
          (let ((term (mod (* (aref comb-row j)
                              (aref bernoulli-numbers j)
                              (mod-expt n-mod (- k+1 j) *modulus*))
                           *modulus*)))
            (setf sum (mod (+ sum term) *modulus*))))
        (mod (* sum (mod-inv k+1)) *modulus*))))

(defun get-primorials (limit)
  "Computes primorials L_k up to limit."
  (let ((primes nil)
        (is-prime (make-array (1+ limit) :element-type 'boolean :initial-element t))
        (res (make-array (1+ limit))))
    (setf (aref is-prime 0) nil (aref is-prime 1) nil)
    (loop for p from 2 to limit do
      (when (aref is-prime p)
        (push p primes)
        (loop for i from (* p p) to limit by p do (setf (aref is-prime i) nil))))
    (setf primes (sort primes #'<))
    (setf (aref res 0) 1)
    (loop for k from 1 to limit do
      (let ((prod 1))
        (dolist (p primes)
          (if (<= p k) (setf prod (* prod p)) (return)))
        (setf (aref res k) prod)))
    res))

(defun solve (&optional (n-limit #.(expt 10 18)))
  (let* ((max-k 60) ; L_53 > 10^18
         (primorials (get-primorials max-k))
         (bernoulli (compute-bernoulli max-k))
         (fact-inv (make-fixnum-array (1+ max-k))))
    
    (setf (aref fact-inv 0) 1)
    (loop for i from 1 to max-k do
      (setf (aref fact-inv i) (mod (* (aref fact-inv (1- i)) (mod-inv i)) *modulus*)))

    (let ((total-sum 0))
      (iterate ((k (scan-range :from 0 :upto max-k)))
        (let ((lk (aref primorials k)))
          (when (<= lk (- n-limit k))
            (let* ((km (floor (- n-limit k) lk))
                   (s-km (faulhaber-sum km k bernoulli))
                   (lk-mod (mod lk *modulus*))
                   (coeff (mod (* (if (evenp k) 1 -1)
                                  (mod-expt lk-mod k *modulus*)
                                  (aref fact-inv k))
                               *modulus*))
                   (term (mod (* coeff s-km) *modulus*)))
              (setf total-sum (mod (+ total-sum term) *modulus*))
              ;; Optional: Debug log for small n
              (when (<= n-limit 10)
                (format t "k=~D, Lk=~D, Km=~D, Coeff=~D, S=~D, Term=~D~%" 
                        k lk km coeff s-km term))))))
      (format t "Final Sum for N=~D: ~D~%" n-limit total-sum)
      total-sum)))

#+| Do it | (project-euler-0969:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Final Sum for N=1000000000000000000: 412543690

User time    =        0.002
System time  =        0.000
Elapsed time =        0.002
Allocation   = 65104 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 412543690
:ok

