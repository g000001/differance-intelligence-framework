;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0729 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0729)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))
(declaim (optimize (speed 3) (safety 0) (debug 0)))
;; ------------------------------------------------------------
;; Iterated Function System (IFS) with Reverse Composition
;; ------------------------------------------------------------

(defun eval-g-and-deriv (S start-idx k x)
  "Evaluates G_S(x) and its derivative using the chain rule. 
   CRITICAL FIX: Functions must be applied from right to left (i = k down to 1)."
  (let ((v x)
        (deriv 1.0d0))
    (loop for i from 1 to k do
      (let* ((idx (mod (- (+ start-idx k) i) k))
             (sign (aref S idx)))
        ;; Apply g_+(v) or g_-(v)
        (setf v (* 0.5d0 (+ v (* sign (sqrt (+ (* v v) 4.0d0))))))
        ;; Chain rule: g'(x) = y^2 / (y^2 + 1) where y = g(x)
        (setf deriv (* deriv (/ (* v v) (+ (* v v) 1.0d0))))))
    (values v deriv)))

(defun find-fixed-point-newton (S start-idx k)
  "Finds the fixed point of the strict contraction using Newton's method."
  (let ((x 0.0d0))
    (loop for iter from 0 below 50 do
      (multiple-value-bind (v deriv) (eval-g-and-deriv S start-idx k x)
        (let ((diff (- v x)))
          (if (< (abs diff) 1d-14)
              (return x)
              (setf x (- x (/ diff (- deriv 1.0d0))))))))
    x))

(defun find-max-shift-idx (w k)
  "Finds the starting index of the lexicographically largest cyclic shift."
  (let ((best-start 0))
    (loop for i from 1 below k do
      (loop for j from 0 below k do
        (let* ((idx1 (mod (+ i j) k))
               (idx2 (mod (+ best-start j) k))
               (c1 (aref w idx1))
               (c2 (aref w idx2)))
          (cond ((> c1 c2) (setf best-start i) (return))
                ((< c1 c2) (return))))))
    best-start))

;; ------------------------------------------------------------
;; Combinatorial Topology & Bijective Projection
;; ------------------------------------------------------------

(defun generate-lyndon (k action)
  "Generates Lyndon words of length exactly k using FKM Algorithm."
  (let ((w (make-array (1+ k) :initial-element 0))
        (copy (make-array k)))
    (labels ((backtrack (t-idx p)
               (if (> t-idx k)
                   (when (= k p)
                     (dotimes (i k)
                       (setf (aref copy i) (if (zerop (aref w (1+ i))) -1.0d0 1.0d0)))
                     (funcall action copy))
                   (progn
                     (setf (aref w t-idx) (aref w (- t-idx p)))
                     (backtrack (1+ t-idx) p)
                     (loop for j from (1+ (aref w (- t-idx p))) to 1 do
                       (setf (aref w t-idx) j)
                       (backtrack (1+ t-idx) t-idx))))))
      ;; Alphabet {0, 1}. Start with 0 ensures generation of Lyndon words.
      (setf (aref w 1) 0)
      (backtrack 2 1))))

;; ------------------------------------------------------------
;; Main Solver API
;; ------------------------------------------------------------

(defun solve-s (limit-p)
  "Evaluates S(P) over the collapsed phase space of Lyndon orbits."
  (let ((total-s 0.0d0))
    (loop for k from 2 to limit-p do
      (generate-lyndon k
        (lambda (w)
          ;; Lyndon word is the min-shift, so start-idx = 0 yields the minimum element
          (let* ((max-idx (find-max-shift-idx w k))
                 (x-min (find-fixed-point-newton w 0 k))
                 (x-max (find-fixed-point-newton w max-idx k)))
            (incf total-s (* k (- x-max x-min)))))))
    total-s))

(defun solve ()
  "Entry point for Project Euler 729."
  (format t "Trace S(2) = ~,4F (Expected 2.8284)~%" (solve-s 2))
  (format t "Trace S(3) = ~,4F (Expected 14.6461)~%" (solve-s 3))
  (format t "Trace S(5) = ~,4F (Expected 124.1056)~%" (solve-s 5))
  
  (let* ((ans (solve-s 25))
         (formatted (format nil "~,4F" ans)))
    (format t "S(25) = ~A~%" formatted)
    formatted))

#+| Do it | (project-euler-0729:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Trace S(2) = 2.8284 (Expected 2.8284)
Trace S(3) = 14.6461 (Expected 14.6461)
Trace S(5) = 124.1056 (Expected 124.1056)
S(25) = 308896374.2502

User time    =  0:01:26.349
System time  =        1.747
Elapsed time =  0:01:54.363
Allocation   = 71935159920 bytes
14744 Page faults
GC time      =        0.777
 |------------------------------------------------------------|#
;;→ "308896374.2502"
:ok