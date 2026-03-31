;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0478 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0478)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun mod-expt (base-val power-val divisor-val)
  (let ((result 1)
        (current-base (mod base-val divisor-val))
        (current-power power-val))
    (do ()
        ((<= current-power 0))
      (when (= (logand current-power 1) 1)
        (setf result (mod (* result current-base) divisor-val)))
      (setf current-base (mod (* current-base current-base) divisor-val))
      (setf current-power (ash current-power -1)))
    result))

(defun sieve (limit-n)
  (let* ((primes (make-array 700000 :element-type 'fixnum :adjustable t :fill-pointer 0))
         (prime-flags (make-array (1+ limit-n) :element-type 'bit :initial-element 1))
         (mu-array (make-array (1+ limit-n) :element-type '(signed-byte 8) :initial-element 0))
         (phi-array (make-array (1+ limit-n) :element-type '(unsigned-byte 32) :initial-element 0)))
    (setf (aref prime-flags 0) 0 (aref prime-flags 1) 0)
    (setf (aref mu-array 1) 1 (aref phi-array 1) 1)
    (do ((index-i 2 (1+ index-i)))
        ((> index-i limit-n))
      (when (= (aref prime-flags index-i) 1)
        (vector-push-extend index-i primes)
        (setf (aref mu-array index-i) -1)
        (setf (aref phi-array index-i) (1- index-i)))
      (let ((num-primes (fill-pointer primes)))
        (do ((index-j 0 (1+ index-j)))
            ((>= index-j num-primes))
          (let* ((prime-p (aref primes index-j))
                 (prod-val (* index-i prime-p)))
            (when (> prod-val limit-n) (return))
            (setf (aref prime-flags prod-val) 0)
            (if (= (mod index-i prime-p) 0)
                (progn
                  (setf (aref mu-array prod-val) 0)
                  (setf (aref phi-array prod-val) (* (aref phi-array index-i) prime-p))
                  (return))
                (progn
                  (setf (aref mu-array prod-val) (- (aref mu-array index-i)))
                  (setf (aref phi-array prod-val) (* (aref phi-array index-i) (1- prime-p)))))))))
    (values mu-array phi-array)))

(defun compute-cm (index-m limit-n mu-array)
  (let ((sum-points 0)
        (limit-d (floor limit-n index-m)))
    (iterate ((index-d (scan-range :from 1 :upto limit-d)))
      (let ((mu-val (aref mu-array index-d)))
        (when (/= mu-val 0)
          (let ((val-k (floor limit-n (* index-m index-d))))
            (incf sum-points (* mu-val (- (* val-k (floor limit-n index-d))
                                          (* index-m (ash (* val-k (1+ val-k)) -1)))))))))
    (1+ sum-points)))

(defun compute-n-tot (limit-n mu-array psi-mod)
  (let ((sum-n-tot 0))
    (iterate ((index-d (scan-range :from 1 :upto limit-n)))
      (let ((mu-val (aref mu-array index-d)))
        (when (/= mu-val 0)
          (let* ((term-v (1+ (floor limit-n index-d)))
                 (term-v-mod (mod term-v psi-mod))
                 (term-v2 (mod (* term-v-mod term-v-mod) psi-mod))
                 (term-v3 (mod (* term-v2 term-v-mod) psi-mod))
                 (term-v3-minus-1 (mod (1- term-v3) psi-mod)))
            (setf sum-n-tot (mod (+ sum-n-tot (* mu-val term-v3-minus-1)) psi-mod))))))
    (mod sum-n-tot psi-mod)))

(defun compute-e-fail-val (limit-n mu-array phi-array n-tot-psi mod-val phi-mod psi-mod)
  (let ((e-fail 1))
    (iterate ((index-m (scan-range :from 1 :upto limit-n)))
      (let* ((val-cm (compute-cm index-m limit-n mu-array))
             (cm-psi (mod val-cm psi-mod))
             (diff-val (mod (- n-tot-psi 1 (* 2 cm-psi)) psi-mod))
             (val-pm (floor diff-val 2))
             (pow-cm (mod-expt 2 (mod cm-psi phi-mod) mod-val))
             (pow-pm (mod-expt 2 (mod val-pm phi-mod) mod-val))
             (term-1 (mod (- pow-cm 1) mod-val))
             (term-prod (mod (* term-1 pow-pm) mod-val))
             (phi-m-mod (mod (aref phi-array index-m) mod-val))
             (coeff-val (mod (* 6 phi-m-mod) mod-val)))
        (setf e-fail (mod (+ e-fail (* coeff-val term-prod)) mod-val))))
    e-fail))

(defun solve-euler-478 (&optional (limit-n 10000000))
  (multiple-value-bind (mu-array phi-array) (sieve limit-n)
    (let* ((mod-val 214358881)
           (phi-mod 194871710)
           (psi-mod (* 2 phi-mod))
           (n-tot-psi (compute-n-tot limit-n mu-array psi-mod))
           (e-fail (compute-e-fail-val limit-n mu-array phi-array n-tot-psi mod-val phi-mod psi-mod))
           (n-tot-phi (mod n-tot-psi phi-mod))
           (total-subsets (mod-expt 2 n-tot-phi mod-val)))
      (mod (- total-subsets e-fail) mod-val))))

(defun solve ()
  (format t "Testing N=1: ~A~%" (solve-euler-478 1))
  (format t "Testing N=2: ~A~%" (solve-euler-478 2))
  (format t "Testing N=10: ~A~%" (solve-euler-478 10))
  (format t "Testing N=500: ~A~%" (solve-euler-478 500))
  (format t "Solving for N=10,000,000...~%")
  (let ((ans (solve-euler-478 10000000)))
    (format t "Answer: ~A~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Testing N=1: 103
Testing N=2: 520447
Testing N=10: 82608406
Testing N=500: 13801403
Solving for N=10,000,000...
Answer: 59510340

User time    =       21.415
System time  =        0.068
Elapsed time =       21.343
Allocation   = 80030368 bytes
13452 Page faults
GC time      =        0.005
 |------------------------------------------------------------|#
;;→ 59510340
:ok