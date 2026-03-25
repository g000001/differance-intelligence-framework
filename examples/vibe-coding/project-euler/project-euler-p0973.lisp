;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0973-refactored (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0973-refactored)

(defun precompute-powers-of-two (n modulo)
  "Precomputes powers of 2 modulo M up to n."
  (let ((pow2 (make-array (1+ n) :element-type 'fixnum :initial-element 0)))
    (setf (aref pow2 0) 1)
    (iterate (for i from 1 to n)
      (setf (aref pow2 i) (mod (* 2 (aref pow2 (1- i))) modulo)))
    pow2))

(defun evaluate-recurrence (n b-val modulo)
  "Evaluates the linear recurrence h_k for a given bit b >= 1."
  (let ((h (make-array (1+ n) :element-type 'fixnum :initial-element 0)))
    (setf (aref h 0) 1)
    (iterate (for k from 1 to n)
      (let ((c-k 0))
        (when (= k 1) (decf c-k))
        (when (= k b-val) (incf c-k))
        (when (= k (1+ b-val)) (decf c-k))
        
        (let ((val c-k))
          (setf val (mod (+ val (* 2 (aref h (1- k)))) modulo))
          (when (>= k b-val)
            (setf val (mod (- val (* 3 (aref h (- k b-val)))) modulo)))
          (when (>= k (1+ b-val))
            (setf val (mod (+ val (* 2 (aref h (- k b-val 1)))) modulo)))
          (setf (aref h k) val))))
    h))

(defun compute-bit-contribution (n b b-val pow2-array modulo inv2)
  "Computes the number of compositions where an odd number of parts have bit b set."
  (cond
    ((= b 0)
     ;; Base case: b = 0 degenerates into an alternating sequence
     (let* ((sign (if (evenp n) 1 -1))
            (h-n (mod (* sign (aref pow2-array (1- n))) modulo)))
       (mod (* (- (aref pow2-array (1- n)) h-n) inv2) modulo)))
    (t
     ;; General case: b >= 1 uses the linear recurrence
     (let* ((h-array (evaluate-recurrence n b-val modulo))
            (h-n (aref h-array n)))
       (mod (* (- (aref pow2-array (1- n)) h-n) inv2) modulo)))))

(defun solve ()
  "Calculates X(n) modulo 10^9+7 by summing bitwise contributions of compositions."
  (let* ((n 10000)
         (modulo 1000000007)
         (inv2 500000004)
         (pow2-array (precompute-powers-of-two n modulo))
         (total-expected-xor 0))
    
    (iterate (for b from 0 to 13)
      (let* ((b-val (ash 1 b))
             (c-b (compute-bit-contribution n b b-val pow2-array modulo inv2)))
        (setf total-expected-xor (mod (+ total-expected-xor (* b-val c-b)) modulo))))
        
    (mod (- total-expected-xor (mod n 2)) modulo)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.022
System time  =        0.001
Elapsed time =        0.012
Allocation   = 1133136 bytes
100 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 427278142
:ok