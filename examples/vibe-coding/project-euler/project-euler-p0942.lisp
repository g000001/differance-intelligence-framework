;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0942 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0942)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant $modulus 1000000007)

;; ------------------------------------------------------------
;; Zero-Cost Abstraction Array Generators
;; ------------------------------------------------------------

(defun make-s8-array (size)
  (make-array size :element-type '(signed-byte 8) :initial-element 0))

(defun make-u32-array (size)
  (make-array size :element-type '(unsigned-byte 32) :initial-element 0))

;; ------------------------------------------------------------
;; Exact Arithmetic & Cryptographic Operations
;; ------------------------------------------------------------

(defun power-mod (base exp m)
  (declare (type (unsigned-byte 64) base exp m))
  (labels ((pow (b e r)
             (declare (type (unsigned-byte 64) b e r))
             (if (zerop e)
                 r
                 (pow (mod (* b b) m)
                      (ash e -1)
                      (if (oddp e) (mod (* r b) m) r)))))
    (pow (mod base m) exp 1)))

(defun jacobi (a n)
  "Computes Jacobi symbol (a/n) in O(log n) using bitwise shifts."
  (declare (type (unsigned-byte 32) a n))
  (labels ((j-rec (a n t-val)
             (declare (type (unsigned-byte 32) a n)
                      (type (signed-byte 8) t-val))
             (if (zerop a)
                 (if (= n 1) t-val 0)
                 (let ((a-val a)
                       (t-new t-val))
                   (labels ((remove-2s (a-in t-in)
                              (declare (type (unsigned-byte 32) a-in)
                                       (type (signed-byte 8) t-in))
                              (if (oddp a-in)
                                  (values a-in t-in)
                                  (let ((mod8 (logand n 7)))
                                    (remove-2s (ash a-in -1)
                                               (if (or (= mod8 3) (= mod8 5))
                                                   (the (signed-byte 8) (- t-in))
                                                   t-in))))))
                     (multiple-value-bind (a-odd t-odd) (remove-2s a-val t-new)
                       (declare (type (unsigned-byte 32) a-odd)
                                (type (signed-byte 8) t-odd))
                       (let* ((next-t (if (= (logand a-odd 3) (logand n 3) 3)
                                          (the (signed-byte 8) (- t-odd))
                                          t-odd))
                              (next-a (mod n a-odd))
                              (next-n a-odd))
                         (j-rec next-a next-n next-t))))))))
    (j-rec (mod a n) n 1)))

;; ------------------------------------------------------------
;; Main Solver API & Dimensional Collapse Vector
;; ------------------------------------------------------------

(defun solve-r (q)
  "Calculates minimal root R(q) modulo 10^9+7 via Gauss sum projection."
  (declare (type (unsigned-byte 32) q))
  (let* ((L (make-s8-array q))
         (primes (make-u32-array #.(* 45 (expt 10 5))))
         (prime-cnt 0)
         (sum-s 0)
         (pow2 4))
    (declare (type (simple-array (signed-byte 8) (*)) L)
             (type (simple-array (unsigned-byte 32) (*)) primes)
             (type (unsigned-byte 32) prime-cnt)
             (type (unsigned-byte 64) sum-s pow2))
             
    (setf (aref L 1) 1)
    
    ;; Phase 1: Linear Sieve for exact Legendre/Jacobi spectra
    (iterate ((i (scan-range :from 2 :below (1- q))))
      (let ((li (aref L i)))
        (declare (type (signed-byte 8) li))
        (when (zerop li)
          (setf (aref primes prime-cnt) i)
          (incf prime-cnt)
          (let ((j (jacobi i q)))
            (declare (type (signed-byte 8) j))
            (setf li j)
            (setf (aref L i) j)))
            
        (iterate ((idx (scan-range :from 0 :below prime-cnt)))
          (let* ((p (aref primes idx))
                 (ip (* i p)))
            (declare (type (unsigned-byte 32) p)
                     (type (unsigned-byte 64) ip))
            (if (>= ip q)
                (terminate-producing)
                (progn
                  (setf (aref L ip) (* li (aref L p)))
                  (when (zerop (mod i p))
                    (terminate-producing))))))))
                    
    ;; Phase 2: Amalgamation of positive Gauss sum exponents
    (iterate ((a (scan-range :from 1 :below (1- q))))
      (when (= (aref L a) 1)
        (setf sum-s (mod (+ sum-s pow2) $modulus)))
      (setf pow2 (mod (ash pow2 1) $modulus)))
      
    ;; Phase 3: Topological Mirror Resolution based on mod 8 state
    (let* ((2-to-q (power-mod 2 q $modulus))
           (X (mod (+ sum-s 2) $modulus))
           (p-minus-X (mod (- 2-to-q 3 sum-s) $modulus))
           (mod8 (logand q 7)))
      ;; q ≡ 1 (mod 8) forces p-X to be smaller. q ≡ 5 (mod 8) favors X.
      (if (= mod8 1)
          (mod (+ p-minus-X $modulus) $modulus)
          (mod (+ X $modulus) $modulus)))))

(defun solve ()
  "Entry point for Project Euler 942."
  ;; Defensive Trace Execution verifying the Isomorphism
  (format t "Trace R(5) = ~D (Expected 6)~%" (solve-r 5))
  (format t "Trace R(17) = ~D (Expected 47569)~%" (solve-r 17))
  
  ;; Final Manifestation using Zero-Cost Abstraction
  (let* ((target-q 74207281)
         (ans (solve-r target-q)))
    (format t "R(~D) mod 10^9+7 = ~D~%" target-q ans)
    ans))

#+| Do it | (project-euler-0942:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace R(5) = 6 (Expected 6)
Trace R(17) = 47569 (Expected 47569)
R(74207281) mod 10^9+7 = 557539756

User time    =        4.871
System time  =        0.094
Elapsed time =        4.841
Allocation   = 130703184 bytes
34202 Page faults
GC time      =        0.003
 |------------------------------------------------------------|#
;;→ 557539756
:ok