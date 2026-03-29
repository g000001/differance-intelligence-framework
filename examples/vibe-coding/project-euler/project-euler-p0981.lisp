;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0981 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0981)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defconstant $modulus 888888883)
(defconstant $limit 88)

;; ------------------------------------------------------------
;; Exact Integer Arithmetic & Generator Utilities
;; ------------------------------------------------------------

(defun make-uint64-array (array-size)
  "Creates an array optimized for 64-bit unsigned integers."
  (make-array array-size :element-type '(unsigned-byte 64) :initial-element 0))

(defun compute-mod-inverse (value modulo-value)
  "Computes the modular inverse using Extended Euclidean Algorithm."
  (labels ((ext-gcd (a b)
             (if (zerop b)
                 (values 1 0 a)
                 (multiple-value-bind (q r) (truncate a b)
                   (multiple-value-bind (s t-val gcd) (ext-gcd b r)
                     (values t-val (- s (* q t-val)) gcd))))))
    (multiple-value-bind (x y gcd) (ext-gcd value modulo-value)
      (declare (ignore y gcd))
      (mod x modulo-value))))

(defun is-same-parity? (number-a number-b)
  "Returns true if both numbers have the same parity."
  (eq (evenp number-a) (evenp number-b)))

(defun compute-multinomial (count-x count-y count-z fact-array inv-fact-array mod-value)
  "Computes the multinomial coefficient (X+Y+Z)! / (X!Y!Z!) modulo M."
  (let* ((total-sum (+ count-x count-y count-z))
         (numerator (aref fact-array total-sum))
         (den-x (aref inv-fact-array count-x))
         (den-y (aref inv-fact-array count-y))
         (den-z (aref inv-fact-array count-z))
         (denom-xy (mod (* den-x den-y) mod-value))
         (denom-xyz (mod (* denom-xy den-z) mod-value)))
    (mod (* numerator denom-xyz) mod-value)))

;; ------------------------------------------------------------
;; Structural Invariant Evaluation
;; ------------------------------------------------------------

(defun calc-neutral-count (count-x count-y count-z fact-array inv-fact-array mod-value)
  "Projects the sequence to the fixed point of the Neutral string logic."
  (let ((total-strings (compute-multinomial count-x count-y count-z fact-array inv-fact-array mod-value))
        (inverse-two (compute-mod-inverse 2 mod-value)))
    (if (and (evenp count-x) (evenp count-y) (evenp count-z))
        (let* ((half-x (ash count-x -1))
               (half-y (ash count-y -1))
               (half-z (ash count-z -1))
               (sign-value (if (evenp (+ half-x half-y half-z)) 1 -1))
               (signed-strings (compute-multinomial half-x half-y half-z fact-array inv-fact-array mod-value))
               (adjusted-signed (mod (* sign-value signed-strings) mod-value))
               (sum-strings (mod (+ total-strings adjusted-signed) mod-value)))
          (mod (* sum-strings inverse-two) mod-value))
        ;; For all-odd case, the inversion parity cancels perfectly to 0.
        (mod (* total-strings inverse-two) mod-value))))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Computes the sum of N(i^3, j^3, k^3) utilizing symmetry."
  (let* ((max-val (* 3 (expt (1- $limit) 3)))
         ;; Mathematical defense: Pad the array size slightly to neutralize any +1 overreach in macro expansion
         (array-size (+ max-val 10))
         (fact-array (make-uint64-array array-size))
         (inv-fact-array (make-uint64-array array-size)))
         
    ;; Precomputation Phase (O(N) exact generation)
    (setf (aref fact-array 0) 1)
    (iterate ((index-i (scan-range :from 1 :upto max-val)))
      (setf (aref fact-array index-i) (mod (* (aref fact-array (1- index-i)) index-i) $modulus)))
      
    (setf (aref inv-fact-array max-val) (compute-mod-inverse (aref fact-array max-val) $modulus))
    
    ;; Bug Fix: Explicitly bind :by -1 to prevent positive step evaluation
    (iterate ((index-i (scan-range :from (1- max-val) :downto 0 :by -1)))
      (setf (aref inv-fact-array index-i) (mod (* (aref inv-fact-array (1+ index-i)) (1+ index-i)) $modulus)))

    ;; Trace examples before manifestation
    (format t "Trace N(2,2,2) = ~A (Expected: 42)~%" 
            (calc-neutral-count 2 2 2 fact-array inv-fact-array $modulus))
    (format t "Trace N(8,8,8) = ~A (Expected: 4732773210 -> ~A mod 888888883)~%" 
            (calc-neutral-count 8 8 8 fact-array inv-fact-array $modulus)
            (mod 4732773210 $modulus))

    ;; Bijective Generation Loop
    (let ((total-sum 0))
      (iterate ((index-i (scan-range :from 0 :below $limit)))
        (iterate ((index-j (scan-range :from index-i :below $limit)))
          (when (is-same-parity? index-i index-j)
            (iterate ((index-k (scan-range :from index-j :below $limit)))
              (when (is-same-parity? index-i index-k)
                (let* ((x-val (expt index-i 3))
                       (y-val (expt index-j 3))
                       (z-val (expt index-k 3))
                       (n-val (calc-neutral-count x-val y-val z-val fact-array inv-fact-array $modulus))
                       (multiplier (cond ((and (= index-i index-j) (= index-j index-k)) 1)
                                         ((or (= index-i index-j) (= index-j index-k)) 3)
                                         (t 6)))
                       (added-term (mod (* n-val multiplier) $modulus)))
                  (setf total-sum (mod (+ total-sum added-term) $modulus))))))))
      (format t "Ans = ~A~%" total-sum)
      total-sum)))

#+| Do it | (project-euler-0981:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace N(2,2,2) = 42 (Expected: 42)
Trace N(8,8,8) = 288328795 (Expected: 4732773210 -> 288328795 mod 888888883)
Ans = 794963735

User time    =        0.281
System time  =        0.033
Elapsed time =        0.254
Allocation   = 31837440 bytes
10770 Page faults
GC time      =        0.017
 |------------------------------------------------------------|#
;;→ 794963735
:ok
