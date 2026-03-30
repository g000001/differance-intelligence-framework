;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0471 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0471)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))
(declaim (optimize (speed 3) (safety 0) (debug 0)))
;; ------------------------------------------------------------
;; Exact Integer Arithmetic & Dimension Collapse
;; ------------------------------------------------------------

(defun sum-k0 (limit-l limit-r)
  "Calculates the sum of 1 from limit-l to limit-r."
  (if (> limit-l limit-r) 0 (1+ (- limit-r limit-l))))

(defun sum-k1 (limit-l limit-r)
  "Calculates the sum of k from limit-l to limit-r."
  (if (> limit-l limit-r) 0
      (/ (* (sum-k0 limit-l limit-r) (+ limit-l limit-r)) 2)))

(defun sum-k2 (limit-l limit-r)
  "Calculates the sum of k^2 from limit-l to limit-r exactly."
  (if (> limit-l limit-r) 0
      (- (/ (* limit-r (1+ limit-r) (+ (* 2 limit-r) 1)) 6)
         (/ (* (1- limit-l) limit-l (+ (* 2 limit-l) -1)) 6))))

(defun calc-l-approx (target-n terms)
  "Computes the rational Taylor expansion of ln(1 - 1/n) securely without floats."
  (collect-sum
   (mapping ((index-j (scan-range :from 1 :upto terms)))
     (/ -1 (* index-j (expt target-n index-j))))))

(defun calc-bernoulli-diff (target-n)
  "Computes the exact rational difference of Euler-Maclaurin remainder terms D(n)."
  (- (+ (/ 1 (* 2 target-n))
        (/ 1 (* 120 (expt target-n 4)))
        (/ 1 (* 240 (expt target-n 8)))
        (/ 691 (* 32760 (expt target-n 12))))
     (+ (/ 1 (* 12 (expt target-n 2)))
        (/ 1 (* 252 (expt target-n 6)))
        (/ 1 (* 132 (expt target-n 10))))))

(defun compute-ln2-ratio (terms)
  "Computes an exact rational approximation of ln(2) exceeding 100 digits of precision."
  (* 2 (collect-sum
        (mapping ((index-k (scan-range :from 0 :upto terms)))
          (/ 1 (* (1+ (* 2 index-k)) (expt 3 (1+ (* 2 index-k)))))))))

(defun exact-harmonic-diff (limit-n)
  "Calculates the exact harmonic difference for small n."
  (let ((mid-m (floor limit-n 2)))
    (collect-sum
     (mapping ((index-k (scan-range :from (1+ mid-m) :upto (1- limit-n))))
       (/ 1 index-k)))))

(defun approx-harmonic-diff (limit-n terms)
  "Asymptotic projection of harmonic difference using Taylor and Euler-Maclaurin expansions."
  (let* ((mid-m (floor limit-n 2))
         (ln2-ratio (compute-ln2-ratio 150))
         (l-approx-ratio (calc-l-approx limit-n terms))
         (d-n-minus-1 (calc-bernoulli-diff (1- limit-n)))
         (d-mid-m (calc-bernoulli-diff mid-m))
         (delta-e (- d-n-minus-1 d-mid-m)))
    (+ ln2-ratio l-approx-ratio delta-e)))

;; ------------------------------------------------------------
;; Structural Invariant Search
;; ------------------------------------------------------------

(defun solve-g (limit-n)
  "Calculates G(n) completely in the Ratio domain (Exact Integer Projection)."
  (let* ((mid-m (floor limit-n 2))
         ;; Region 1: k <= floor(n/2) -> Collapses perfectly to (k^2 - 1)/6
         (sum-region-1 (- (/ (sum-k2 2 mid-m) 6) (/ (sum-k0 2 mid-m) 6)))
         ;; Region 2: k > floor(n/2) -> Decomposes into exact polynomials and harmonic series
         (sum-region-poly (+ (* 5/6 (sum-k2 (1+ mid-m) (1- limit-n)))
                             (* -1 (+ (* 2 limit-n) 1) (sum-k1 (1+ mid-m) (1- limit-n)))
                             (* (/ (+ (* 9 limit-n limit-n) (* 9 limit-n) 1) 6) 
                                (sum-k0 (1+ mid-m) (1- limit-n)))))
         (coeff-c (/ (* limit-n (1+ limit-n) (+ (* 2 limit-n) 1)) 6))
         (harmonic-diff (if (<= limit-n 1000)
                            (exact-harmonic-diff limit-n)
                            (approx-harmonic-diff limit-n 10))))
    (- (+ sum-region-1 sum-region-poly) (* coeff-c harmonic-diff))))

;; ------------------------------------------------------------
;; Scientific Format Extractor (Zero-Float Guarantee)
;; ------------------------------------------------------------

(defun format-scientific (ratio-val sig-digits)
  "Extracts scientific notation directly from the Ratio domain without floating-point degradation."
  (labels ((scale-down (current-val current-e)
             (if (>= current-val 10)
                 (scale-down (/ current-val 10) (1+ current-e))
                 (values current-val current-e)))
           (scale-up (current-val current-e)
             (if (< current-val 1)
                 (scale-up (* current-val 10) (1- current-e))
                 (values current-val current-e))))
    (multiple-value-bind (scaled-val e-val)
        (if (>= ratio-val 10)
            (scale-down ratio-val 0)
            (scale-up ratio-val 0))
      (let* ((multiplier (expt 10 (1- sig-digits)))
             (scaled-integer (* scaled-val multiplier))
             (rounded-integer (round scaled-integer)))
        ;; Guard against rounding overflow (e.g., 9.999999999 -> 10.0)
        (when (>= rounded-integer (* 10 multiplier))
          (setf rounded-integer (round (/ rounded-integer 10)))
          (incf e-val))
        (let* ((str-val (write-to-string rounded-integer))
               (first-digit (subseq str-val 0 1))
               (rest-digits (subseq str-val 1)))
          (format nil "~A.~Ae~A" first-digit rest-digits e-val))))))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Computes G(10^11) using O(1) mathematical dimension collapse."
  ;; Defensive Trace Execution against Boundary Conditions
  (format t "Trace G(10)  = ~A (Expected 2.059722222e1)~%" 
          (format-scientific (solve-g 10) 10))
  (format t "Trace G(100) = ~A (Expected 1.922360980e4)~%" 
          (format-scientific (solve-g 100) 10))
  
  ;; Execution for target N
  (let* ((target-n #.(expt 10 11))
         (ans (solve-g target-n))
         (formatted-ans (format-scientific ans 10)))
    (format t "G(10^11) = ~A~%" formatted-ans)
    formatted-ans))

#+| Do it | (project-euler-0471:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace G(10)  = 2.059722222e1 (Expected 2.059722222e1)
Trace G(100) = 1.922360980e4 (Expected 1.922360980e4)
G(10^11) = 1.895093981e31

User time    =        0.006
System time  =        0.000
Elapsed time =        0.004
Allocation   = 352848 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "1.895093981e31"
:ok