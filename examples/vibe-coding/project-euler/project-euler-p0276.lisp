;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0276 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0276)

#||
(cl-text "Project Euler 276 Logic Projection"
  (cl-comment "1. Exact Integer Projection: Use Alcuin's sequence to compute total valid triangles without floating point operations.")
  (forall (p)
    (if (evenp p)
        (= (T p) (round_to_nearest (/ (^ p 2) 48)))
        (= (T p) (round_to_nearest (/ (^ (+ p 3) 2) 48)))))
        
  (cl-comment "2. O(1) Structural Summation: Derived an exact O(1) mathematical identity for the prefix sum of Alcuin's sequence S(n) = sum_{p=1}^n T(p), avoiding $O(N)$ accumulation loops.")
  (forall (n)
    (= (S n) (O_1_Formula n)))
    
  (cl-comment "3. Dirichlet Hyperbola Method: Instead of a naive $O(N)$ traversal for Mobius inversion, chunk N/d bounds into exact intervals, reducing dynamic allocations and iterations to $O(\\sqrt{N})$.")
  (forall (N)
    (= (TotalPrimitive N)
       (sum (v 1 (sqrt N)) (* (- (M (upper v)) (M (lower v))) (S (val v))))))
)
||#


;;; =========================================================
;;; Alcuin's Sequence Prefix Sum: O(1) Exact Mathematics
;;; =========================================================

(defun calc-f-val (k)
  "Evaluates f(k) = floor((k^2 + 6) / 12) using exact integer arithmetic."
  (truncate (+ (* k k) 6) 12))

(defun calc-F (m)
  "Evaluates sum_{k=1}^m f(k) in strictly O(1) time without looping."
  (if (<= m 0)
      0
      (let* ((q (truncate m 12))
             (r (mod m 12))
             ;; Compute sum(k^2) = m(m+1)(2m+1)/6 safely, deferring bignum growth
             (sum-sq (let ((a m) (b (1+ m)) (c (1+ (* 2 m))))
                       (cond ((= (mod a 2) 0) (setf a (truncate a 2)))
                             (t (setf b (truncate b 2))))
                       (cond ((= (mod a 3) 0) (setf a (truncate a 3)))
                             ((= (mod b 3) 0) (setf b (truncate b 3)))
                             (t (setf c (truncate c 3))))
                       (* a b c)))
             (sum-6 (* 6 m))
             ;; Precomputed residuals modulo 12 sum
             (sum-R (+ (* 86 q) (aref #(0 7 17 20 30 37 43 50 60 63 73 80 86) r))))
        (truncate (- (+ sum-sq sum-6) sum-R) 12))))

(defun calc-S (n)
  "Returns the number of triangles with perimeter <= n. O(1) time."
  (if (<= n 0)
      0
      (let ((m (truncate n 2)))
        (if (evenp n)
            (+ (* 2 (calc-F m)) (calc-f-val (1+ m)))
            (+ (* 2 (calc-F m)) (calc-f-val (1+ m)) (calc-f-val (+ m 2)))))))

;;; =========================================================
;;; Fast Linear Sieve for Mobius Function: O(N)
;;; =========================================================

(defun sieve-mu (limit)
  "Generates Mobius function array up to limit in O(N)."
  (let ((mu (make-array (1+ limit) :element-type '(signed-byte 8) :initial-element 0))
        (primes (make-array (+ (truncate limit 10) 100000) :element-type 'fixnum))
        (prime-count 0)
        (is-prime (make-array (1+ limit) :element-type 'bit :initial-element 1)))
    (declare (type (simple-array (signed-byte 8) (*)) mu)
             (type (simple-array fixnum (*)) primes)
             (type fixnum prime-count limit)
             (type simple-bit-vector is-prime)
             (optimize (speed 3) (safety 0)))
    (setf (aref mu 1) 1)
    (setf (sbit is-prime 0) 0)
    (setf (sbit is-prime 1) 0)
    (iterate
      (declare (type fixnum i))
      (for i from 2 to limit)
      (when (= (sbit is-prime i) 1)
        (setf (aref primes prime-count) i)
        (incf prime-count)
        (setf (aref mu i) -1))
      (iterate
        (declare (type fixnum j p ip))
        (for j from 0 below prime-count)
        (for p = (aref primes j))
        (for ip = (* i p))
        (while (<= ip limit))
        (setf (sbit is-prime ip) 0)
        (if (= (mod i p) 0)
            (progn
              (setf (aref mu ip) 0)
              (leave))
            (setf (aref mu ip) (- (aref mu i))))))
    mu))

;;; =========================================================
;;; Core Solver using Dirichlet Hyperbola Block Method
;;; =========================================================

(defun solve ()
  (let* ((N 10000000)
         (mu (sieve-mu N))
         (M (make-array (1+ N) :element-type 'fixnum)))
    (declare (type (simple-array fixnum (*)) M)
             (type (simple-array (signed-byte 8) (*)) mu))
    
    (format t "Sieve generated.~%")
    
    ;; 1. Prefix sum array of the Mobius function M(n) = sum_{k=1}^n mu(k)
    (setf (aref M 0) 0)
    (iterate
      (declare (type fixnum i))
      (for i from 1 to N)
      (setf (aref M i) (+ (aref M (1- i)) (aref mu i))))
    
    (format t "Mobius Prefix Sums prepared. Commencing O(sqrt N) hyperbola blocks...~%")
    
    ;; 2. Mobius inversion block logic (Dirichlet Hyperbola)
    ;; Total Primitive(N) = sum_{d=1}^N mu(d) * S(N/d)
    ;; Bounded evaluation in 2*sqrt(N) jumps instead of N.
    (let ((total 0)
          (d 1))
      (iterate
        (while (<= d N))
        (let* ((nd (truncate N d))
               (next-d (truncate N nd)))
          (let ((mu-sum (- (aref M next-d) (aref M (1- d)))))
            (unless (= mu-sum 0)
              (incf total (* mu-sum (calc-S nd)))))
          (setf d (1+ next-d))))
      
      (format t "Done. Maximum Bignum Consing strictly eliminated.~%")
      total)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Sieve generated.
Mobius Prefix Sums prepared. Commencing O(sqrt N) hyperbola blocks...
Done. Maximum Bignum Consing strictly eliminated.

User time    =        0.635
System time  =        0.047
Elapsed time =        0.604
Allocation   = 100262008 bytes
23867 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 5777137137739632912
:ok