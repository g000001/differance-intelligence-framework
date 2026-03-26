;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0689 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0689)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
  Gil-Pelaez Inversion Formula with Optimized Series
  - Integration range: T=400, M=200, Steps=100000.
  - Series scan leverages strict type bounds '(simple-array double-float (*))
    to bridge declarative mathematics with bare-metal memory efficiency.
||#

(defmacro make-double-float-array (size)
  `(make-array ,size :element-type 'double-float :initial-element 0d0))

(defun calculate-residual-rn (term-count)
  "Calculates the residual tail sum R_N = sum_{i=N+1}^infty 1/i^4 using Series."
  (- (/ (expt pi 4) 90d0)
     (collect-sum
      (mapping ((i (scan-range :from 1 :upto term-count)))
        (/ 1d0 (expt i 4))))))

(defun build-divisors-array (term-count)
  "Precomputes 2*i^2 using Series collection."
  (collect '(simple-array double-float (*))
    (mapping ((i (scan-range :from 1 :upto term-count)))
      (* 2d0 i i))))

(defun evaluate-integrand (time-t residual-rn constant-k divisors-array)
  "Evaluates the characteristic integrand. The infinite product is collapsed 
   using a strictly typed Series scan to avoid generic dispatch overhead."
  (declare (type double-float time-t residual-rn constant-k)
           (type (simple-array double-float (*)) divisors-array)
           (optimize (speed 3) (safety 0)))
  (if (= time-t 0d0)
      constant-k
      (let* ((product (collect-fn 'double-float (constantly 1d0)
                                  (lambda (acc div)
                                    (declare (type double-float acc div))
                                    (* acc (cos (/ time-t div))))
                                  (scan '(simple-array double-float (*)) divisors-array)))
             (exp-term (exp (* -0.125d0 time-t time-t residual-rn)))
             (sin-term (sin (* constant-k time-t))))
        (declare (type double-float product exp-term sin-term))
        (* (/ sin-term time-t) product exp-term))))

(defun perform-simpson-integration (max-time step-count residual-rn constant-k divisors-array)
  "Performs numerical integration using Simpson's 1/3 rule with declarative Series."
  (declare (type double-float max-time residual-rn constant-k)
           (type fixnum step-count)
           (optimize (speed 3) (safety 0)))
  (let* ((step-size (/ max-time step-count 1.0d0))
         (boundary-sum (+ constant-k
                          (evaluate-integrand max-time residual-rn constant-k divisors-array))))
    (declare (type double-float step-size boundary-sum))
    (let ((sum (collect-sum
                 (mapping ((i (scan-range :from 1 :below step-count)))
                   (let* ((t-val (* i step-size))
                          (y (evaluate-integrand t-val residual-rn constant-k divisors-array))
                          (weight (if (oddp i) 4d0 2d0)))
                     (declare (type double-float t-val y weight))
                     (* weight y))))))
      (declare (type double-float sum))
      (* (+ boundary-sum sum) (/ step-size 3d0)))))

(defun solve ()
  (let* ((term-count 200)
         (max-time 400d0)
         (step-count 100000)
         (constant-k (- (/ (expt pi 2) 12d0) 0.5d0))
         (residual-rn (calculate-residual-rn term-count))
         (divisors-array (build-divisors-array term-count)))
    (format t "-> Evaluating Gil-Pelaez characteristic integral with optimized Series...~%")
    (format t "-> Max Time: ~F, Steps: ~D, Truncation M: ~D~%" max-time step-count term-count)
    (let* ((integral-value (perform-simpson-integration max-time step-count residual-rn constant-k divisors-array))
           (probability (+ 0.5d0 (/ integral-value pi))))
      (format t "-> Integral computed.~%")
      (format nil "~,8F" probability))))

#+| Do it | (project-euler-0689:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
-> Evaluating Gil-Pelaez characteristic integral with optimized Series...
-> Max Time: 400.0, Steps: 100000, Truncation M: 200
-> Integral computed.

User time    =        0.630
System time  =        0.019
Elapsed time =        0.591
Allocation   = 1305738768 bytes
1314 Page faults
GC time      =        0.014
 |------------------------------------------------------------|#
;;→ "0.56565454"
:ok