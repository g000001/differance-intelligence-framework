;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0382 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0382)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

;; ------------------------------------------------------------
;; Exact Arithmetic & Matrix Isomorphism
;; ------------------------------------------------------------

(defun matrix-multiply (matrix-a matrix-b modulus)
  "Multiplies two 9x9 matrices under a given modulo using strict Exact Integer logic."
  (let ((result-matrix (make-array '(9 9) :initial-element 0)))
    (iterate ((index-i (scan-range :from 0 :below 9)))
      (iterate ((index-j (scan-range :from 0 :below 9)))
        (let ((cell-sum 0))
          (iterate ((index-k (scan-range :from 0 :below 9)))
            (setf cell-sum (mod (+ cell-sum 
                                   (* (aref matrix-a index-i index-k) 
                                      (aref matrix-b index-k index-j))) 
                                modulus)))
          (setf (aref result-matrix index-i index-j) cell-sum))))
    result-matrix))

(defun matrix-power (base-matrix target-exponent modulus)
  "Computes (base-matrix ^ target-exponent) mod modulus in O(log N)."
  (let ((result-matrix (make-array '(9 9) :initial-element 0))
        (current-base (make-array '(9 9))))
    ;; Initialize result as Identity Matrix and copy the base
    (iterate ((index-i (scan-range :from 0 :below 9)))
      (iterate ((index-j (scan-range :from 0 :below 9)))
        (setf (aref result-matrix index-i index-j) (if (= index-i index-j) 1 0))
        (setf (aref current-base index-i index-j) (aref base-matrix index-i index-j))))
    
    (labels ((power-recursive (current-exp curr-base curr-result)
               (if (zerop current-exp)
                   curr-result
                   (let ((next-result (if (oddp current-exp)
                                          (matrix-multiply curr-result curr-base modulus)
                                          curr-result))
                         (next-base (matrix-multiply curr-base curr-base modulus)))
                     (power-recursive (ash current-exp -1) next-base next-result)))))
      (power-recursive target-exponent current-base result-matrix))))

(defun matrix-vector-multiply (matrix state-vector modulus)
  "Applies the transition matrix to the state vector."
  (let ((result-vector (make-array 9 :initial-element 0)))
    (iterate ((index-i (scan-range :from 0 :below 9)))
      (let ((row-sum 0))
        (iterate ((index-j (scan-range :from 0 :below 9)))
          (setf row-sum (mod (+ row-sum 
                                (* (aref matrix index-i index-j) 
                                   (aref state-vector index-j))) 
                             modulus)))
        (setf (aref result-vector index-i) row-sum)))
    result-vector))

;; ------------------------------------------------------------
;; Phase Space Definition
;; ------------------------------------------------------------

(defun get-transition-matrix ()
  "Returns the deterministic transformation matrix defining the Alethetic Leap.
   Vector Layout: [K_n, K_{n-1}, K_{n-2}, E_n, E_{n-1}, E_{n-2}, 2^{n-2}, 1, f(n)]"
  (make-array '(9 9) :initial-contents
              '((0  0  1  0 -1  0  2  1  0)  ; K_n
                (1  0  0  0  0  0  0  0  0)  ; K_{n-1}
                (0  1  0  0  0  0  0  0  0)  ; K_{n-2}
                (0 -1  0  0  0  1  2 -1  0)  ; E_n
                (0  0  0  1  0  0  0  0  0)  ; E_{n-1}
                (0  0  0  0  1  0  0  0  0)  ; E_{n-2}
                (0  0  0  0  0  0  2  0  0)  ; 2^{n-2}
                (0  0  0  0  0  0  0  1  0)  ; 1
                (0  0 -1  0  1  0  2 -1  1)))) ; f(n)

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Entry point for Project Euler 382."
  (let* ((modulus #.(expt 10 9))
         (target-n #.(expt 10 18))
         (transition-matrix (get-transition-matrix))
         ;; Evaluated state at n=4
         (initial-state (make-array 9 :initial-contents '(6 4 2 1 0 0 4 1 2))))
    
    ;; Boundary Validation
    (let* ((matrix-10 (matrix-power transition-matrix (- 10 4) modulus))
           (state-10 (matrix-vector-multiply matrix-10 initial-state modulus)))
      (format t "Trace f(10) = ~A (Expected 501)~%" (aref state-10 8)))
      
    (let* ((matrix-25 (matrix-power transition-matrix (- 25 4) modulus))
           (state-25 (matrix-vector-multiply matrix-25 initial-state modulus)))
      (format t "Trace f(25) = ~A (Expected 18635853)~%" (aref state-25 8)))
      
    ;; Final Manifestation
    (let* ((matrix-target (matrix-power transition-matrix (- target-n 4) modulus))
           (state-target (matrix-vector-multiply matrix-target initial-state modulus))
           (answer (aref state-target 8)))
      (format t "f(10^18) mod 10^9 = ~A~%" answer)
      answer)))

#+| Do it | (project-euler-0382:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace f(10) = 501 (Expected 501)
Trace f(25) = 18635853 (Expected 18635853)
f(10^18) mod 10^9 = 697003956

User time    =        0.008
System time  =        0.000
Elapsed time =        0.005
Allocation   = 96216 bytes
5 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 697003956
:ok
