;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0689 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0689)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
  Gil-Pelaez Inversion Formula with Series.
  - Integration range: T=400 to eliminate truncation error.
  - Dimension collapse: Inner product truncated at M=200 with exponential residual.
  - Expressed entirely in declarative Series macros, fully leveraging 
    modern Lisp's allocation tolerance and GC performance.
||#

(defconstant +pi^4/90+ (/ (expt pi 4) 90d0))

(defmacro make-double-float-array (size)
  `(make-array ,size :element-type 'double-float :initial-element 0d0))

(defun calculate-residual-rn (term-count)
  "Calculates the residual tail sum R_N = sum_{i=N+1}^infty 1/i^4 using Series."
  (- +pi^4/90+
     (collect-sum
       (mapping ((i (scan-range :from 1 :upto term-count)))
         (/ 1d0 (expt (coerce i 'double-float) 4))))))

(defun build-divisors-array (term-count)
  "Precomputes 2*i^2 using Series collection."
  (collect '(simple-array double-float (*))
    (mapping ((i (scan-range :from 1 :upto term-count)))
      (* 2d0 (coerce i 'double-float) i))))

(defun evaluate-integrand (time-t residual-rn constant-k divisors-array)
  "Evaluates the characteristic integrand. The infinite product is collapsed 
   using collect-fn for mapping over the precomputed divisors."
  (declare (type double-float time-t residual-rn constant-k)
           (type (simple-array double-float (*)) divisors-array)
           (optimize (speed 3) (safety 0)))
  (if (= time-t 0d0)
      constant-k
      (let* ((product (collect-fn 'double-float
                                  (constantly 1d0)
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
  (let* ((step-size (/ max-time (coerce step-count 'double-float)))
         (boundary-sum (+ constant-k
                          (evaluate-integrand max-time residual-rn constant-k divisors-array))))
    (declare (type double-float step-size boundary-sum))
    (let ((sum (collect-sum
                 (mapping ((i (scan-range :from 1 :below step-count)))
                   (let* ((t-val (* (coerce i 'double-float) step-size))
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
    (format t "-> Evaluating Gil-Pelaez characteristic integral with Series...~%")
    (format t "-> Max Time: ~F, Steps: ~D, Truncation M: ~D~%" max-time step-count term-count)
    (let* ((integral-value (perform-simpson-integration max-time step-count residual-rn constant-k divisors-array))
           (probability (+ 0.5d0 (/ integral-value pi))))
      (format t "-> Integral computed.~%")
      (format nil "~,8F" probability))))

#+| Do it | (project-euler-0689:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
-> Evaluating Gil-Pelaez characteristic integral with Series...
-> Max Time: 400.0, Steps: 100000, Truncation M: 200
-> Integral computed.

User time    =        0.612
System time  =        0.019
Elapsed time =        0.572
Allocation   = 1306029480 bytes
3788 Page faults
GC time      =        0.013
 |------------------------------------------------------------|#
;;→ "0.56565454"
:ok