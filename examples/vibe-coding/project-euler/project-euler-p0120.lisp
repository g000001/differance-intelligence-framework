;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0120 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0120)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)

(defun solve ()
  "Calculates the sum of r_max for 3 <= a <= 1000.
The solution uses the derived formula:
If a is odd, r_max = a * (a - 1)
If a is even, r_max = a * (a - 2)"
  (let (($limit-a 1000))
    (format t "Starting Project Euler P120 with limit a = ~D...~%" $limit-a)
    (let ((total-sum
            (iterate (for a from 3 to $limit-a)
              ;; Mathematical Jump:
              ;; r = ((a-1)^n + (a+1)^n) mod a^2
              ;; For even n, r = 2 mod a^2.
              ;; For odd n, r = 2na mod a^2.
              ;; To maximize 2na mod a^2, we maximize k = 2n mod a.
              ;; If a is odd, max k = a - 1.
              ;; If a is even, max k = a - 2.
              (let ((r-max (if (oddp a)
                               (* a (- a 1))
                               (* a (- a 2)))))
                ;; Debugging output for verification of initial values
                (when (<= a 10)
                  (format t "a = ~3D, r_max = ~6D~%" a r-max))
                (sum r-max)))))
      (format t "Calculation complete.~%")
      (format t "Result: ~D~%" total-sum)
      total-sum)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Project Euler P120 with limit a = 1000...
a =   3, r_max =      6
a =   4, r_max =      8
a =   5, r_max =     20
a =   6, r_max =     24
a =   7, r_max =     42
a =   8, r_max =     48
a =   9, r_max =     72
a =  10, r_max =     80
Calculation complete.
Result: 333082500

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 3712 bytes
10 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 333082500
:ok
