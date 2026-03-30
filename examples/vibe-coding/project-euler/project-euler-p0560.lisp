;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0560 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0560)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +mod+ 1000000007)

;; ------------------------------------------------------------
;; Exact Arithmetic & Inverse Fields
;; ------------------------------------------------------------

(defun power-mod (base exp)
  "Computes (base^exp) mod 10^9+7 safely."
  (declare (type (unsigned-byte 64) base exp))
  (labels ((pow (b e r)
             (declare (type (unsigned-byte 64) b e r))
             (if (zerop e)
                 r
                 (pow (mod (* b b) +mod+)
                      (ash e -1)
                      (if (oddp e) (mod (* r b) +mod+) r)))))
    (pow (mod base +mod+) exp 1)))

(defun mod-inverse (n)
  "Fermat's Little Theorem for modular inverse."
  (power-mod n (- +mod+ 2)))

(defun next-power-of-2 (n)
  "Calculates the required safe boundary M to prevent aliasing."
  (declare (type (unsigned-byte 32) n))
  (if (<= n 1) 1
      (ash 1 (integer-length (1- n)))))

;; ------------------------------------------------------------
;; Phase Space Definition (Linear Sieve)
;; ------------------------------------------------------------

(defun build-c-array (limit-n)
  "Builds the frequency array of Grundy values in O(N)."
  (declare (type (unsigned-byte 32) limit-n))
  (let* ((lpf (make-array limit-n :element-type '(unsigned-byte 32) :initial-element 0))
         (primes (make-array 700000 :element-type '(unsigned-byte 32)))
         (pi-val (make-array limit-n :element-type '(unsigned-byte 32) :initial-element 0))
         (prime-count 0))
    (declare (type (unsigned-byte 32) prime-count))
    
    (setf (aref primes prime-count) 2)
    (incf prime-count)
    (setf (aref pi-val 2) prime-count)
    
    ;; Linear Sieve for O(N) LPF factorization
    (iterate ((i (scan-range :from 3 :below limit-n :by 2)))
      (when (zerop (aref lpf i))
        (setf (aref lpf i) i)
        (setf (aref primes prime-count) i)
        (incf prime-count)
        (setf (aref pi-val i) prime-count))
      (iterate ((j (scan-range :from 1 :below prime-count)))
        (let ((p (aref primes j)))
          (when (or (> p (aref lpf i)) (>= (* p i) limit-n))
            (terminate-producing))
          (setf (aref lpf (* p i)) p))))
          
    ;; The mathematical projection of the problem constraints
    (let* ((size (next-power-of-2 (1+ prime-count)))
           (c-arr (make-array size :element-type '(unsigned-byte 64) :initial-element 0)))
      (setf (aref c-arr 0) (ash (1- limit-n) -1))
      (setf (aref c-arr 1) 1)
      
      (iterate ((i (scan-range :from 3 :below limit-n :by 2)))
        (let ((p (aref lpf i)))
          (incf (aref c-arr (aref pi-val p)))))
          
      (values c-arr size))))

;; ------------------------------------------------------------
;; Fast Walsh-Hadamard Transform (FWHT)
;; ------------------------------------------------------------

(defun fwht (a size)
  "Applies FWHT in O(M log M) replacing polynomial convolution."
  (declare (type (simple-array (unsigned-byte 64) (*)) a)
           (type (unsigned-byte 32) size))
  (let ((res (make-array size :element-type '(unsigned-byte 64))))
    (iterate ((i (scan-range :from 0 :below size)))
      (setf (aref res i) (mod (aref a i) +mod+)))
    (let ((len 1))
      (declare (type (unsigned-byte 32) len))
      (iterate ((_ (scan-range :from 0 :below (integer-length (1- size)))))
        (let ((step (ash len 1)))
          (declare (type (unsigned-byte 32) step))
          (iterate ((i (scan-range :from 0 :below size :by step)))
            (iterate ((j (scan-range :from 0 :below len)))
              (let* ((idx1 (+ i j))
                     (idx2 (+ i len j))
                     (u (aref res idx1))
                     (v (aref res idx2))
                     (sum (mod (+ u v) +mod+))
                     ;; (+ ... +mod+) eliminates negative modulo hallucination
                     (diff (mod (+ (- u v) +mod+) +mod+)))
                (setf (aref res idx1) sum)
                (setf (aref res idx2) diff))))
          (setf len step)))
      res)))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve-l (n k)
  "Calculates L(n, k) using structural invariant mappings."
  (declare (type (unsigned-byte 32) n k))
  (multiple-value-bind (c-arr size) (build-c-array n)
    (let ((transformed (fwht c-arr size)))
      ;; Projection of exponentiation across the orthogonal domain
      (iterate ((i (scan-range :from 0 :below size)))
        (setf (aref transformed i) (power-mod (aref transformed i) k)))
      
      ;; Inverse FWHT and normalization
      (let* ((inverse-transformed (fwht transformed size))
             (inv-size (mod-inverse size)))
        (mod (* (aref inverse-transformed 0) inv-size) +mod+)))))

(defun solve ()
  "Entry point for Project Euler 560."
  ;; Defensive Trace Execution against Boundary Conditions
  (format t "Trace L(5, 2) = ~A (Expected 6)~%" (solve-l 5 2))
  (format t "Trace L(10, 5) = ~A (Expected 9964)~%" (solve-l 10 5))
  (format t "Trace L(10, 10) = ~A (Expected 472400303)~%" (solve-l 10 10))
  (format t "Trace L(10^3, 10^3) = ~A (Expected 954021836)~%" (solve-l 1000 1000))
  
  (let* ((target-n 10000000)
         (target-k 10000000)
         (ans (solve-l target-n target-k)))
    (format t "L(10^7, 10^7) mod 1,000,000,007 = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0560:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace L(5, 2) = 6 (Expected 6)
Trace L(10, 5) = 9964 (Expected 9964)
Trace L(10, 10) = 472400303 (Expected 472400303)
Trace L(10^3, 10^3) = 954021836 (Expected 954021836)
L(10^7, 10^7) mod 1,000,000,007 = 994345168

User time    =        2.119
System time  =        0.116
Elapsed time =        2.117
Allocation   = 121686832 bytes
47623 Page faults
GC time      =        0.067
 |------------------------------------------------------------|#
;;→ 994345168
:ok
