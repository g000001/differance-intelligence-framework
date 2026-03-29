;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0980 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0980)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

;; ------------------------------------------------------------
;; Generator Utilities & Descriptive Arrays
;; ------------------------------------------------------------

(defun make-uint64-array (array-size &optional (initial-element 0))
  "Creates an array optimized for 64-bit unsigned integers."
  (make-array array-size :element-type '(unsigned-byte 64) :initial-element initial-element))

;; Quaternion Multiplication Table (Flat 1D mapping for absolute speed)
;; States: 0=1, 1=i, 2=j, 3=k, 4=-1, 5=-i, 6=-j, 7=-k
(defconstant $quat-mul-table
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(0 1 2 3 4 5 6 7
     1 4 3 6 5 0 7 2
     2 7 4 1 6 3 0 5
     3 2 5 4 7 6 1 0
     4 5 6 7 0 1 2 3
     5 0 7 2 1 4 3 6
     6 3 0 5 2 7 4 1
     7 6 1 0 3 2 5 4)))

;; ------------------------------------------------------------
;; Main Structural Solver API
;; ------------------------------------------------------------

(defun solve-for-limit (limit-count)
  "Evaluates the quaternion reduction of the sequence and computes neutral pairs."
  (let ((quaternion-frequencies (make-uint64-array 8 0))
        (lookup-table $quat-mul-table)
        (current-quaternion 0))
    (declare (type (simple-array (unsigned-byte 8) (64)) lookup-table)
             (type fixnum current-quaternion))
    
    ;; Alethetic Leap: Evaluate 50*N characters flatly without nested loops or allocations
    (iterate ((current-a-val (scan-fn '(unsigned-byte 64)
                                      (constantly 88888888)
                                      (lambda (previous-val)
                                        (declare (type (unsigned-byte 64) previous-val))
                                        (mod (* 8888 previous-val) 888888883))))
              (step-index (scan-range :from 1 :upto (* 50 limit-count))))
      (declare (type (unsigned-byte 64) current-a-val)
               (type fixnum step-index))
      
      (let* ((remainder-val (mod current-a-val 3))
             ;; Maps modulo {0, 1, 2} precisely to Quaternions {i(1), j(2), k(3)}
             (quaternion-val (1+ remainder-val)))
        (declare (type fixnum remainder-val quaternion-val))
        
        ;; State machine transition: Q_new = Q_old * char
        (setf current-quaternion (aref lookup-table (+ (ash current-quaternion 3) quaternion-val)))
        
        ;; Every 50 characters, a string c(i) is complete. Commit to frequency distribution.
        (when (zerop (mod step-index 50))
          (incf (aref quaternion-frequencies current-quaternion))
          (setf current-quaternion 0))))
    
    ;; Intermediate logic observability
    (format t "Quaternion Frequencies for N=~D: ~A~%" limit-count quaternion-frequencies)
    
    ;; The final reduction: F(N) relies on complementary pairs Q(A) * Q(B) = 1
    (let ((total-neutral-pairs 0))
      (declare (type (unsigned-byte 64) total-neutral-pairs))
      (incf total-neutral-pairs (* (aref quaternion-frequencies 0) (aref quaternion-frequencies 0))) ; 1 * 1 = 1
      (incf total-neutral-pairs (* (aref quaternion-frequencies 4) (aref quaternion-frequencies 4))) ; -1 * -1 = 1
      (incf total-neutral-pairs (* 2 (aref quaternion-frequencies 1) (aref quaternion-frequencies 5))) ; i * -i = 1 (and reverse)
      (incf total-neutral-pairs (* 2 (aref quaternion-frequencies 2) (aref quaternion-frequencies 6))) ; j * -j = 1
      (incf total-neutral-pairs (* 2 (aref quaternion-frequencies 3) (aref quaternion-frequencies 7))) ; k * -k = 1
      
      (format t "F(~D) = ~D~%" limit-count total-neutral-pairs)
      total-neutral-pairs)))

(defun solve ()
  "Entry point for Project Euler 980"
  ;; Execute verification for given examples first to prevent hallucination
  (solve-for-limit 10)
  (solve-for-limit 100)
  ;; Execute the final massive state
  (solve-for-limit #.(expt 10 6)))

#+| Do it | (project-euler-0980:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Quaternion Frequencies for N=10: #(1 2 2 1 0 2 0 2)
F(10) = 13
Quaternion Frequencies for N=100: #(12 14 7 16 12 15 14 10)
F(100) = 1224
Quaternion Frequencies for N=1000000: #(124869 124822 125497 124996 124801 125415 124772 124828)
F(1000000) = 124999683766

User time    =        5.249
System time  =        0.051
Elapsed time =        5.243
Allocation   = 127488 bytes
360 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 124999683766
:ok