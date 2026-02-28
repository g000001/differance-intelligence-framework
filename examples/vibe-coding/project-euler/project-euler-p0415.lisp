;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0415 (:use cl #|alexandria|#))
(in-package #:project-euler-0415)

#||
(cl-text Project-Euler-415-Aletheic-Analysis

  (cl-comment "=== 1. NMF Avoidance (非中道の誤謬の回避) ===")
  (cl-comment "Searching over combinations of points or counting lines individually is O(2^V) or O(N^4).")
  (cl-comment "This is computationally impossible for N=10^11. NMF demands we discard")
  (cl-comment "this worldly attachment and jump to a global, structural invariant.")
  (forall (a p N)
    (if (and (greater_than N 1000000) (iterates_points a))
        (NMF a)))

  (cl-comment "=== 2. ACX Jump (勝義諦への跳躍): Sylvester-Gallai Projection ===")
  (cl-comment "By the Sylvester-Gallai theorem, a finite set of points has no line passing through")
  (cl-comment "exactly two points IF AND ONLY IF all points in the set are collinear, or |S| <= 1.")
  (cl-comment "Thus, titanic sets are exactly the total subsets minus collinear sets and small sets!")
  (equal (T N)
         (sub (sub (pow 2 V) 1 V) (Sum_Collinear_Ge3)))

  (cl-comment "=== 3. Bijective Generation of Lines (対称性の破れ) ===")
  (cl-comment "Collinear subsets of size >= 3 are partitioned by the unique maximal line containing them.")
  (cl-comment "Summing f(k) = 2^k - 1 - k - choose(k, 2) over all maximal lines prevents overcounting.")
  (equal (Sum_Collinear_Ge3)
         (sum_L (sub (pow 2 (length L)) 1 (length L) (choose (length L) 2))))

  (cl-comment "=== 4. Exact Integer Projection and Mobius Inversion ===")
  (cl-comment "The line generation is mapped to slope steps (a,b) where gcd(a,b)=1.")
  (cl-comment "Applying Mobius inversion isolates the sum into Dirichlet convolution forms")
  (cl-comment "of S0(K), S1(K), S2(K), projecting O(N^2) loops into O(N^(2/3)) prefix evaluation.")
  (forall (a)
    (implies (OptimizedAlgorithm a)
             (uses_mobius_inversion a)))

  (cl-comment "=== 5. Debt Clearance (状態の負債の清算) ===")
  (cl-comment "We precompute Dirichlet sums S0, S1, S2 up to M=3e7.")
  (cl-comment "For K > M, recursive evaluation with memoization is used via block summation,")
  (cl-comment "clearing all redundant recursive trees and leaving minimal GC pressure.")
)
||#


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+quicklisp (ql:quickload :iterate :silent t))
(use-package :iterate)

(defparameter *M* 30000000 "Threshold for linear sieve precomputation")
(defvar *phi* nil)
(defvar *primes* nil)
(defvar *S0* nil)
(defvar *S1* nil)
(defvar *S2* nil)
(defvar *memo-S0* nil)
(defvar *memo-S1* nil)
(defvar *memo-S2* nil)

(defun build-sieve ()
  "Manifests the base arithmetic functions (phi) and their weighted prefix sums (S0, S1, S2)."
  (setf *phi* (make-array (1+ *M*) :element-type '(unsigned-byte 32)))
  (setf *primes* (make-array (floor *M* 10) :element-type '(unsigned-byte 32) :fill-pointer 0))
  (setf *S0* (make-array (1+ *M*) :element-type '(unsigned-byte 32)))
  (setf *S1* (make-array (1+ *M*) :element-type '(unsigned-byte 32)))
  (setf *S2* (make-array (1+ *M*) :element-type '(unsigned-byte 32)))
  
  (setf (aref *phi* 1) 1)
  (iter (declare (type fixnum i idx p))
        (for i from 2 to *M*)
        (when (= (aref *phi* i) 0)
          (vector-push i *primes*)
          (setf (aref *phi* i) (1- i)))
        (iter (for p in-vector *primes*)
              (for idx = (* i p))
              (while (<= idx *M*))
          (if (= (mod i p) 0)
              (progn
                (setf (aref *phi* idx) (* (aref *phi* i) p))
                (finish))
              (setf (aref *phi* idx) (* (aref *phi* i) (1- p))))))
  
  ;; Prefix sums (modulo 10^8)
  (let ((s0 0) (s1 0) (s2 0))
    (declare (type fixnum s0 s1 s2))
    (iter (declare (type fixnum i phi))
          (for i from 1 to *M*)
          (for phi = (aref *phi* i))
          (setf s0 (mod (+ s0 phi) 100000000))
          (setf s1 (mod (+ s1 (mod (* i phi) 100000000)) 100000000))
          (let ((i2phi (mod (* (mod (* i i) 100000000) phi) 100000000)))
            (setf s2 (mod (+ s2 i2phi) 100000000)))
          (setf (aref *S0* i) s0)
          (setf (aref *S1* i) s1)
          (setf (aref *S2* i) s2))))

(defun P1 (K)
  "Sum of i for i=1..K"
  (let* ((K-mod (mod K 100000000))
         (K1-mod (mod (1+ K) 100000000)))
    (if (evenp K)
        (mod (* (ash K-mod -1) K1-mod) 100000000)
        (mod (* K-mod (ash K1-mod -1)) 100000000))))

(defun P2 (K)
  "Sum of i^2 for i=1..K"
  (let ((a K)
        (b (1+ K))
        (c (1+ (* 2 K))))
    (if (evenp a) (setf a (ash a -1)) (setf b (ash b -1)))
    (cond ((= (mod a 3) 0) (setf a (floor a 3)))
          ((= (mod b 3) 0) (setf b (floor b 3)))
          (t (setf c (floor c 3))))
    (let ((res (mod (* (mod a 100000000) (mod b 100000000)) 100000000)))
      (mod (* res (mod c 100000000)) 100000000))))

(defun P3 (K)
  "Sum of i^3 for i=1..K"
  (let ((p1 (P1 K)))
    (mod (* p1 p1) 100000000)))

(defun get-S0 (K)
  "Recursive evaluation of S0 with block summation."
  (if (<= K *M*)
      (aref *S0* K)
      (or (gethash K *memo-S0*)
          (let ((ans (P1 K))
                (j 2))
            (iter (while (<= j K))
                  (let* ((q (floor K j))
                         (next-j (1+ (floor K q)))
                         (count (- next-j j)))
                    (setf ans (mod (- ans (* count (get-S0 q))) 100000000))
                    (setf j next-j)))
            (setf (gethash K *memo-S0*) (mod ans 100000000))))))

(defun get-S1 (K)
  "Recursive evaluation of S1 with block summation."
  (if (<= K *M*)
      (aref *S1* K)
      (or (gethash K *memo-S1*)
          (let ((ans (P2 K))
                (j 2))
            (iter (while (<= j K))
                  (let* ((q (floor K j))
                         (next-j (1+ (floor K q)))
                         (sum-j (mod (- (P1 (1- next-j)) (P1 (1- j))) 100000000)))
                    (setf ans (mod (- ans (* sum-j (get-S1 q))) 100000000))
                    (setf j next-j)))
            (setf (gethash K *memo-S1*) (mod ans 100000000))))))

(defun get-S2 (K)
  "Recursive evaluation of S2 with block summation."
  (if (<= K *M*)
      (aref *S2* K)
      (or (gethash K *memo-S2*)
          (let ((ans (P3 K))
                (j 2))
            (iter (while (<= j K))
                  (let* ((q (floor K j))
                         (next-j (1+ (floor K q)))
                         (sum-j2 (mod (- (P2 (1- next-j)) (P2 (1- j))) 100000000)))
                    (setf ans (mod (- ans (* sum-j2 (get-S2 q))) 100000000))
                    (setf j next-j)))
            (setf (gethash K *memo-S2*) (mod ans 100000000))))))

(defun expt-mod (base exp)
  "Modular exponentiation safely supporting bignums."
  (let ((res 1)
        (b (mod base 100000000))
        (e exp))
    (iter (while (> e 0))
          (when (oddp e)
            (setf res (mod (* res b) 100000000)))
          (setf e (ash e -1))
          (setf b (mod (* b b) 100000000)))
    res))

(defun sum-g0 (X)
  "Computes Sum_{m=1}^X (2^{m-1} - 1)"
  (if (<= X 0) 0
      (mod (- (expt-mod 2 X) 1 (mod X 100000000)) 100000000)))

(defun sum-g1 (X)
  "Computes Sum_{m=1}^X m(2^{m-1} - 1)"
  (if (<= X 0) 0
      (let ((X-mod (mod X 100000000)))
        (mod (- (+ (* (mod (1- X-mod) 100000000) (expt-mod 2 X)) 1)
                (P1 X))
             100000000))))

(defun sum-g2 (X)
  "Computes Sum_{m=1}^X m^2(2^{m-1} - 1)"
  (if (<= X 0) 0
      (let ((X-mod (mod X 100000000)))
        (mod (- (* (mod (+ (* X-mod X-mod) (* -2 X-mod) 3) 100000000)
                   (expt-mod 2 X))
                3
                (P2 X))
             100000000))))

(defun G0 (L R) (mod (- (sum-g0 R) (sum-g0 (1- L))) 100000000))
(defun G1 (L R) (mod (- (sum-g1 R) (sum-g1 (1- L))) 100000000))
(defun G2 (L R) (mod (- (sum-g2 R) (sum-g2 (1- L))) 100000000))

(defun S-hv (N)
  "Horizontal and vertical maximal lines."
  (let* ((MOD 100000000)
         (N+1 (mod (1+ N) MOD))
         (term1 (expt-mod 2 (1+ N)))
         (term2 (mod (1+ N) MOD))
         (term3 (P1 N))
         (fk (mod (- term1 1 term2 term3) MOD)))
    (mod (* 2 N+1 fk) MOD)))

(defun S-diag (N)
  "Diagonal maximal lines evaluated via collapsed blocks."
  (let ((ans 0)
        (N+1 (mod (1+ N) 100000000))
        (N+1-sq (mod (* (mod (1+ N) 100000000) (mod (1+ N) 100000000)) 100000000))
        (m 1))
    (iter (while (<= m N))
          (let* ((K (floor N m))
                 (next-m (1+ (floor N K)))
                 (L m)
                 (R (1- next-m)))
            (let ((g0 (G0 L R))
                  (g1 (G1 L R))
                  (g2 (G2 L R))
                  (s0 (get-S0 K))
                  (s1 (get-S1 K))
                  (s2 (get-S2 K)))
              (let ((v0 (mod (- (* 2 s0) 1) 100000000))
                    (2v1 (mod (- (* 3 s1) 1) 100000000))
                    (v2 s2))
                (let ((term1 (mod (* N+1-sq v0 g0) 100000000))
                      (term2 (mod (* N+1 2v1 g1) 100000000))
                      (term3 (mod (* v2 g2) 100000000)))
                  (setf ans (mod (+ ans term1 (- term2) term3) 100000000)))))
            (setf m next-m)))
    (mod (* 2 ans) 100000000)))

(defun solve ()
  "Manifests the solution to Project Euler 415."
  (build-sieve)
  ;; Clear state debt from prior REPL evaluations
  (setf *memo-S0* (make-hash-table :test 'eql))
  (setf *memo-S1* (make-hash-table :test 'eql))
  (setf *memo-S2* (make-hash-table :test 'eql))
  
  (let* ((N 100000000000)
         (MOD 100000000)
         (V (* (1+ N) (1+ N)))
         (2^V (expt-mod 2 V))
         (V-mod (mod V MOD))
         (Shv (S-hv N))
         (Sdiag (S-diag N))
         ;; By definition: Total - empty/singleton - Lines
         (ans (mod (- 2^V 1 V-mod Shv Sdiag) MOD)))
    (mod ans MOD)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =       58.091
System time  =        0.557
Elapsed time =       58.871
Allocation   = 493733384 bytes
74268 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 55859742
:ok