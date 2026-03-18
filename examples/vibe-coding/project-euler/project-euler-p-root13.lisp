;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-root-13 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-root-13)

;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-bonus (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-bonus)

(defun solve ()
  (let* ((target-n 13)
         (digits 1000)
         ;; Shift the decimal point by multiplying by 10^(2 * 1000)
         (shifted-n (* target-n (expt 10 (* 2 digits))))
         ;; Compute the exact integer square root
         (root (isqrt shifted-n))
         (digit-sum 0))
    
    (format t "Calculating the sum of the first ~A fractional digits of sqrt(~A)...~%" digits target-n)
    
    ;; Extract exactly 'digits' amount of numbers from the right side
    ;; Since sqrt(13) = 3.605551..., 'root' will be 3605551...
    ;; The lower 1000 digits precisely correspond to the fractional part.
    (iterate (for i from 1 to digits)
      (multiple-value-bind (quotient remainder) (truncate root 10)
        (incf digit-sum remainder)
        (setf root quotient)))
        
    (format t "Done.~%")
    digit-sum))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating the sum of the first 1000 fractional digits of sqrt(13)...
Done.

User time    =        0.000
System time  =        0.000
Elapsed time =        0.001
Allocation   = 253872 bytes
6 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 4588
