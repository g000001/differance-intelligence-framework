;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0645 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0645)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

;; ------------------------------------------------------------
;; Exact Arithmetic & Alethetic Projection
;; ------------------------------------------------------------

(defun format-rounded (ratio-val)
  "Exact rounding to 4 decimal places directly from Ratio domain, preventing float hallucination."
  (let* ((scaled (floor (+ (* ratio-val 10000) 1/2)))
         (int-part (floor scaled 10000))
         (frac-part (mod scaled 10000)))
    (format nil "~D.~4,'0D" int-part frac-part)))

(defun solve-e (limit-d)
  "Computes E(D) exactly using the dimensionally collapsed Beta-integral formula."
  (let ((h-d (collect-sum (mapping ((i (scan-range :from 1 :upto limit-d)))
                            (/ 1 i))))
        (mid (floor limit-d 2))
        (current-u 1)
        (sum-u 0))
    (iterate ((k (scan-range :from 1 :upto mid)))
      (when (> k 1)
        (let ((num (* (+ (- limit-d (* 2 k)) 2)
                      (+ (- limit-d (* 2 k)) 1)))
              (den (* (+ (- limit-d k) 1)
                      (- limit-d k))))
          (setf current-u (* current-u (/ num den)))))
      (incf sum-u (/ current-u k)))
    (* limit-d (- h-d sum-u))))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Computes E(10000) enforcing the 1-minute rule without local optimize declarations."
  ;; Defensive Trace Execution against Boundary Conditions
  (format t "Trace E(2) = ~A (Expected 1.0000)~%" (format-rounded (solve-e 2)))
  (format t "Trace E(5) = ~A (Expected 5.1667 -> 31/6)~%" (format-rounded (solve-e 5)))
  
  (let* ((e-365 (solve-e 365))
         (rounded-365 (format-rounded e-365)))
    (format t "Trace E(365) = ~A (Expected 1174.3501)~%" rounded-365))
  
  ;; Execution for target D
  (let* ((target-d 10000)
         (ans (solve-e target-d))
         (formatted-ans (format-rounded ans)))
    (format t "E(~D) = ~A~%" target-d formatted-ans)
    formatted-ans))

#+| Do it | (project-euler-0645:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace E(2) = 1.0000 (Expected 1.0000)
Trace E(5) = 5.1667 (Expected 5.1667 -> 31/6)
Trace E(365) = 1174.3501 (Expected 1174.3501)
E(10000) = 48894.2174

User time    =       12.005
System time  =        0.147
Elapsed time =       12.071
Allocation   = 14818963544 bytes
11913 Page faults
GC time      =        0.153
 |------------------------------------------------------------|#
;;→ "48894.2174"
:ok