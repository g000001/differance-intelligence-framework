;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0441 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0441)

#||
(cl:comment "Project Euler 441: The inverse summation of coprime couples")
(cl:comment "By applying Mobius inversion, changing the order of summations, and leveraging Harmonic numbers,")
(cl:comment "the O(N^2) summation transforms into an O(N log N) algorithm.")
(cl:comment "We group the variable d by the value of M = floor(N/d) (Square Root Decomposition).")
(cl:comment "This reduces the number of times we evaluate the expensive inner sum to O(sqrt(N)).")
(cl:comment "To prevent floating-point precision loss over millions of additions, we implement the Kahan summation algorithm.")
||#

(defun make-harmonic-array (limit)
  "Precomputes Harmonic numbers H_n using Kahan summation for strict precision."
  (let ((harmonic-array (make-array (1+ limit) :element-type 'double-float)))
    (setf (aref harmonic-array 0) 0.0d0)
    (let ((sum 0.0d0)
          (c 0.0d0))
      (declare (type double-float sum c))
      (iterate (for i from 1 to limit)
        (let* ((y (- (/ 1.0d0 i) c))
               (t-sum (+ sum y)))
          (setf c (- (- t-sum sum) y))
          (setf sum t-sum)
          (setf (aref harmonic-array i) sum)))
      harmonic-array)))

(defun make-mobius-array (limit)
  "Generates Mobius function array using a linear sieve."
  (let ((mu (make-array (1+ limit) :element-type 'fixnum :initial-element 0))
        (is-prime (make-array (1+ limit) :element-type 'bit :initial-element 1))
        (primes (make-array (truncate limit 10) :element-type 'fixnum :fill-pointer 0)))
    (setf (aref mu 1) 1)
    (setf (sbit is-prime 0) 0)
    (setf (sbit is-prime 1) 0)
    (iterate (for i from 2 to limit)
      (when (= (sbit is-prime i) 1)
        (vector-push-extend i primes)
        (setf (aref mu i) -1))
      (iterate (for p in-vector primes)
        (when (> (* i p) limit) (finish))
        (setf (sbit is-prime (* i p)) 0)
        (if (zerop (mod i p))
            (progn
              (setf (aref mu (* i p)) 0)
              (finish))
            (setf (aref mu (* i p)) (- (aref mu i))))))
    mu))

(declaim (inline f1-f2))
(defun f1-f2 (m-value limit harmonic-array)
  "Evaluates the inner sums f1 and f2 for a specific M. Uses Kahan summation to avoid precision drift."
  (let ((sum1 0.0d0)
        (sum2 0.0d0)
        (c1 0.0d0)
        (c2 0.0d0)
        (half (truncate (1- m-value) 2))
        (np1 (coerce (1+ limit) 'double-float)))
    (declare (type fixnum m-value limit half)
             (type double-float sum1 sum2 c1 c2 np1)
             (type (simple-array double-float (*)) harmonic-array))
    
    ;; Case 1: 2k <= M-1
    (iterate (for k from 1 to half)
      (let* ((s1-val (- (aref harmonic-array (- m-value k)) (aref harmonic-array k)))
             (s2-val (- (aref harmonic-array m-value) (aref harmonic-array (- m-value k))))
             (v1 (- s1-val 1.0d0))
             (v2 (/ (+ s1-val (* np1 s2-val)) k))
             (y1 (- v1 c1))
             (t1 (+ sum1 y1))
             (y2 (- v2 c2))
             (t2 (+ sum2 y2)))
        (setf c1 (- (- t1 sum1) y1))
        (setf sum1 t1)
        (setf c2 (- (- t2 sum2) y2))
        (setf sum2 t2)))
        
    ;; Case 2: 2k > M-1
    (let ((m-df (coerce m-value 'double-float)))
      (iterate (for k from (1+ half) to (1- m-value))
        (let* ((s2-val (- (aref harmonic-array m-value) (aref harmonic-array k)))
               (v1 (- 1.0d0 (/ m-df k)))
               (v2 (/ (* np1 s2-val) k))
               (y1 (- v1 c1))
               (t1 (+ sum1 y1))
               (y2 (- v2 c2))
               (t2 (+ sum2 y2)))
          (setf c1 (- (- t1 sum1) y1))
          (setf sum1 t1)
          (setf c2 (- (- t2 sum2) y2))
          (setf sum2 t2))))
    (values sum1 sum2)))

(defun solve (&optional (limit 10000000))
  (let* ((half-limit (truncate limit 2))
         (mobius-array (make-mobius-array half-limit))
         (harmonic-array (make-harmonic-array limit))
         (ans 0.0d0)
         (c 0.0d0))
    (declare (type double-float ans c))
    
    (format t "Precomputation done. Starting summation blocks...~%")
    
    (let ((left 1))
      (iterate (while (<= left half-limit))
        (let* ((m-value (truncate limit left))
               (right (if (< m-value 2) half-limit (min half-limit (truncate limit m-value)))))
          (multiple-value-bind (f1 f2) (f1-f2 m-value limit harmonic-array)
            (let ((c1 0.0d0)
                  (c2 0.0d0))
              (declare (type double-float c1 c2))
              ;; Accumulate mobius weights for the current block [left, right]
              (iterate (for d from left to right)
                (let ((mval (aref mobius-array d)))
                  (unless (zerop mval)
                    (let ((md (coerce mval 'double-float))
                          (dd (coerce d 'double-float)))
                      (incf c1 (/ md dd))
                      (incf c2 (/ md (* dd dd)))))))
              ;; Apply results to the global answer via Kahan
              (let* ((v (+ (* c1 f1) (* c2 f2)))
                     (y (- v c))
                     (t-ans (+ ans y)))
                (setf c (- (- t-ans ans) y))
                (setf ans t-ans))))
          (setf left (1+ right)))))
    
    (format t "Done.~%")
    (format nil "~,4F" ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputation done. Starting summation blocks...
Done.

User time    =       14.101
System time  =        0.169
Elapsed time =       14.174
Allocation   = 27735836488 bytes
62551 Page faults
GC time      =        0.258
 |------------------------------------------------------------|#
;;→ "5000088.8395"
:ok