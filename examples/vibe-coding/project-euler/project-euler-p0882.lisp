;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0882 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0882)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
(cl:comment "PE 882 Combinatorial Game Theory and Exact Rational DP")
(cl:comment "Insight 1: The partisan game exactly maps to Conway's Surreal Numbers. G(x) = { max G(x_L) | min G(x_R) }.")
(cl:comment "Insight 2: The previous O(1) guess failed at x=14 because surreal numbers strongly favor integers ({1 | 3} = 2, not 1.5).")
(cl:comment "Shortcut: Since N = 10^5 is small, O(N log N) Dynamic Programming is the intended path. We use Common Lisp's exact `rational` arithmetic to recursively find the simplest dyadic rational between Left and Right options, bypassing complex formulas and guaranteeing 100% mathematical accuracy without float precision limits.")
||#

(defun simplest-between (l r)
  "Finds the simplest surreal number (dyadic rational) strictly between L and R."
  (if (eq r 'infinity)
      (1+ (floor l))
      (let ((int-l (floor l)))
        (if (< int-l (1- (ceiling r)))
            ;; If there's an integer strictly between L and R, pick the smallest one
            (1+ int-l)
            ;; Otherwise, shift, scale by 2, recurse, and scale back
            (let* ((offset int-l)
                   (new-l (- l offset))
                   (new-r (- r offset)))
              (+ offset (/ (simplest-between (* 2 new-l) (* 2 new-r)) 2)))))))

(declaim (inline remove-bit))
(defun remove-bit (x i)
  "Removes the i-th bit from x, shifting higher bits down."
  (declare (type fixnum x i))
  (let ((low (ldb (byte i 0) x))
        (high (ash x (- (1+ i)))))
    (logior (ash high i) low)))

(defun solve-for (n)
  "Dynamically computes G(x) for all x <= n and aggregates the ceiling of their sum."
  (let ((g (make-array (1+ n) :initial-element 0))
        (sum 0))
    (setf (aref g 0) 0)
    (iterate ((x (scan-range :from 1 :upto n)))
      (let ((max-l -1)
            (min-r 'infinity)
            (len (integer-length x)))
        (iterate ((i (scan-range :from 0 :below len)))
          (let ((new-x (remove-bit x i)))
            (if (logbitp i x)
                (let ((val (aref g new-x)))
                  (when (> val max-l)
                    (setf max-l val)))
                (let ((val (aref g new-x)))
                  (when (or (eq min-r 'infinity) (< val min-r))
                    (setf min-r val))))))
        (let ((gx (simplest-between max-l min-r)))
          (setf (aref g x) gx)
          (incf sum (* x gx)))))
    (ceiling sum)))

(defun solve ()
  (format t "Validating True Combinatorial Game Theory logic with S(2)...~%")
  (let ((ans2 (solve-for 2)))
    (format t "S(2) = ~A (Expected 2)~%" ans2)
    (assert (= ans2 2)))
    
  (format t "Validating S(5)...~%")
  (let ((ans5 (solve-for 5)))
    (format t "S(5) = ~A (Expected 17)~%" ans5)
    (assert (= ans5 17)))
    
  (format t "Validating S(10)...~%")
  (let ((ans10 (solve-for 10)))
    (format t "S(10) = ~A (Expected 64)~%" ans10)
    (assert (= ans10 64)))
    
  (format t "Computing S(10^5) with Exact Rational DP...~%")
  (let ((ans (solve-for 100000)))
    (format t "Final Answer S(10^5): ~A~%" ans)
    ans))

#+| Do it | (project-euler-0882:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Validating True Combinatorial Game Theory logic with S(2)...
S(2) = 2 (Expected 2)
Validating S(5)...
S(5) = 17 (Expected 17)
Validating S(10)...
S(10) = 64 (Expected 64)
Computing S(10^5) with Exact Rational DP...
Final Answer S(10^5): 15800662276

User time    =        0.324
System time  =        0.017
Elapsed time =        0.277
Allocation   = 64084912 bytes
4588 Page faults
GC time      =        0.007
 |------------------------------------------------------------|#
;;→ 15800662276
:ok