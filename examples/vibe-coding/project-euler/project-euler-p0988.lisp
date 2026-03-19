;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0988 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0988)

(defun solve (&optional (a 19) (b 53))
  (declare (type fixnum a b))
  
  ;; DP tables for counts (prefix-counts) and sums (prefix-sums)
  ;; Dimensions are (a x b) to handle 1-based indexing easily.
  (let ((prefix-counts (make-array (list a b) :element-type 'integer :initial-element 0))
        (prefix-sums   (make-array (list a b) :element-type 'integer :initial-element 0)))

    (format t "Calculating F(~A, ~A) using 2D Antichain DP...~%" a b)

    (iterate (for y from 1 to (1- a))
      (iterate (for x from 1 to (1- b))
        (let ((val (- (* y b) (* x a)))
              (current-count 0)
              (current-sum 0))
          (declare (type fixnum val))
          
          ;; If the point (y, x) represents a valid gap
          (when (> val 0)
            ;; A valid sequence can start here (+1), or append to any valid sequence strictly smaller in both x and y.
            (setf current-count (1+ (aref prefix-counts (1- y) (1- x))))
            ;; The sum increases by 'val' for every valid sequence it appends to, plus the existing sums.
            (setf current-sum (+ (* current-count val) 
                                 (aref prefix-sums (1- y) (1- x)))))

          ;; Standard 2D prefix sum update for counts
          (setf (aref prefix-counts y x)
                (+ (aref prefix-counts (1- y) x)
                   (aref prefix-counts y (1- x))
                   (- (aref prefix-counts (1- y) (1- x)))
                   current-count))

          ;; Standard 2D prefix sum update for sums
          (setf (aref prefix-sums y x)
                (+ (aref prefix-sums (1- y) x)
                   (aref prefix-sums y (1- x))
                   (- (aref prefix-sums (1- y) (1- x)))
                   current-sum))))
                   
      ;; Intermediate logging for transparency
      (when (or (= y (1- a)) (zerop (mod y 5)))
        (format t "Processed y = ~A / ~A, current accumulated sum = ~A~%"
                y (1- a) (aref prefix-sums y (1- b)))))

    ;; The answer is the total sum accumulated over the entire valid grid
    (let ((ans (aref prefix-sums (1- a) (1- b))))
      (format t "Final ans = ~A~%" ans)
      ans)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating F(19, 53) using 2D Antichain DP...
Processed y = 5 / 18, current accumulated sum = 794437
Processed y = 10 / 18, current accumulated sum = 56446686831
Processed y = 15 / 18, current accumulated sum = 3529161812282699
Processed y = 18 / 18, current accumulated sum = 2727531976556215755
Final ans = 2727531976556215755

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 18000 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 2727531976556215755
:ok