;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0936 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0936)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; ------------------------------------------------------------
;; Exact Arithmetic & Inverse Generating Functions
;; ------------------------------------------------------------

(defun remove-item (dp v m-val)
  "Applies the inverse operator (1 - x^v y)^M to the polynomial."
  (if (zerop m-val)
      dp
      (let ((next-dp (make-array '(55 55) :initial-element 0))
            (c-arr (make-array 55 :initial-element 0)))
        (setf (aref c-arr 0) 1)
        (iterate ((i (scan-range :from 1 :upto 50)))
          (setf (aref c-arr i) (/ (* (aref c-arr (1- i)) (- m-val (1- i))) i)))
        (iterate ((k (scan-range :from 0 :upto 50)))
          (iterate ((w (scan-range :from 0 :upto 50)))
            (let ((sum 0)
                  (i-max (min k (floor w v))))
              (iterate ((i (scan-range :from 0 :upto i-max)))
                (let ((term (aref dp (- k i) (- w (* i v))))
                      (c-val (aref c-arr i)))
                  (unless (or (zerop term) (zerop c-val))
                    (if (evenp i)
                        (incf sum (* term c-val))
                        (decf sum (* term c-val))))))
              (setf (aref next-dp k w) sum))))
        next-dp)))

(defun add-item-in-place (dp v m-val)
  "Applies the composition operator (1 - x^v y)^{-M} to the polynomial."
  (unless (zerop m-val)
    (let ((next-dp (make-array '(55 55) :initial-element 0))
          (c-arr (make-array 55 :initial-element 0)))
      (setf (aref c-arr 0) 1)
      (iterate ((i (scan-range :from 1 :upto 50)))
        (setf (aref c-arr i) (/ (* (aref c-arr (1- i)) (+ m-val i -1)) i)))
      (iterate ((k (scan-range :from 0 :upto 50)))
        (iterate ((w (scan-range :from 0 :upto 50)))
          (let ((sum 0)
                (i-max (min k (floor w v))))
            (iterate ((i (scan-range :from 0 :upto i-max)))
              (let ((term (aref dp (- k i) (- w (* i v))))
                    (c-val (aref c-arr i)))
                (unless (or (zerop term) (zerop c-val))
                  (incf sum (* term c-val)))))
            (setf (aref next-dp k w) sum))))
      (iterate ((k (scan-range :from 0 :upto 50)))
        (iterate ((w (scan-range :from 0 :upto 50)))
          (setf (aref dp k w) (aref next-dp k w)))))))

(defun calc-rev-dp (dp d n a-mat)
  "Creates a parallel timeline DP by removing specific degree constraints."
  (let ((cur-dp dp))
    (iterate ((v (scan-range :from 1 :below n)))
      (let ((m-val (aref a-mat v d)))
        (when (> m-val 0)
          (setf cur-dp (remove-item cur-dp v m-val)))))
    cur-dp))

;; ------------------------------------------------------------
;; Phase Space Projection (Otter's Tree Enumeration)
;; ------------------------------------------------------------

(defun solve-s (limit-n)
  "Evaluates S(N) using structural tree decomposition and DP inverse operators."
  (let ((a-mat (make-array '(55 55) :initial-element 0))
        (t-tree (make-array '(55 55) :initial-element 0))
        (dp (make-array '(55 55) :initial-element 0))
        (p-arr (make-array 55 :initial-element 0)))
    (setf (aref dp 0 0) 1)
    
    (iterate ((n (scan-range :from 1 :upto limit-n)))
      (if (= n 1)
          (progn
            (setf (aref a-mat 1 1) 1)
            (setf (aref t-tree 1 1) 1)
            (add-item-in-place dp 1 1))
          (progn
            ;; Calculate A(n, d) and T(n, d) safely ignoring symmetric constraints
            (iterate ((d (scan-range :from 1 :upto n)))
              (let ((rev-dp (calc-rev-dp dp d n a-mat)))
                (setf (aref a-mat n d) (aref rev-dp (1- d) (1- n)))
                (setf (aref t-tree n d) (aref rev-dp d (1- n)))))
            
            ;; Integrate computed states back into the main timeline
            (iterate ((d (scan-range :from 1 :upto n)))
              (add-item-in-place dp n (aref a-mat n d))))))
              
    ;; Condense local tree states into the global S(N) expectation
    (let ((total-s 0))
      (iterate ((n (scan-range :from 3 :upto limit-n)))
        (let ((t-total 0)
              (e-total 0))
          (iterate ((d (scan-range :from 1 :upto n)))
            (incf t-total (aref t-tree n d)))
          
          (iterate ((v (scan-range :from 1 :below n)))
            (let ((n-v (- n v)))
              (iterate ((c1 (scan-range :from 1 :upto v)))
                (iterate ((c2 (scan-range :from 1 :upto n-v)))
                  (when (/= c1 c2)
                    (incf e-total (* (aref a-mat v c1) (aref a-mat n-v c2))))))))
          ;; Symmetry reduction is exact due to strict inequality bounds
          (setf e-total (/ e-total 2))
          (let ((p-n (- t-total e-total)))
            (setf (aref p-arr n) p-n)
            (incf total-s p-n))))
      total-s)))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve ()
  "Entry point for Project Euler 936."
  ;; Defensive Trace Execution verifying the Isomorphism
  (format t "Trace P(7) = ~A (Expected 6)~%" (- (solve-s 7) (solve-s 6)))
  (format t "Trace S(10) = ~A (Expected 74)~%" (solve-s 10))
  
  ;; Final Manifestation at Level 33+ scale using Zero-Cost Abstraction
  (let ((ans (solve-s 50)))
    (format t "S(50) = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0936:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace P(7) = 6 (Expected 6)
Trace S(10) = 74 (Expected 74)
S(50) = 12144907797522336

User time    =        4.038
System time  =        0.064
Elapsed time =        3.935
Allocation   = 611458464 bytes
5676 Page faults
GC time      =        0.025
 |------------------------------------------------------------|#
;;→ 12144907797522336
:ok