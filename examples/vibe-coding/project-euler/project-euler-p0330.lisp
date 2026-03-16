;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0330 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0330)

#||
(cl:comment "CLIF logic for Project Euler 330")
(cl:text
  (Equivalence (Summation (+ (A n) (B n))) (C n))
  (Recurrence (C n) (- (* n (C (- n 1))) (F n)))
  (Constraint "Modulo M = 77777777 has largest prime factor 137")
  (Equivalence (modulo (Factorial k) 77777777) 0 (for (>= k 137)))
  (Optimization "The sum for C(n) truncates after 137 terms due to P(n, j) being divisible by M")
  (Optimization "F_n mod M can be computed using a polynomial of powers i^n since k! S(n,k) vanishes for k >= 137")
)
||#


(defun power-mod (base exponent modulo-number)
  "Calculates (base^exponent) mod modulo-number efficiently."
  (let ((result 1)
        (current-base (mod base modulo-number))
        (current-exponent exponent))
    (iterate (while (> current-exponent 0))
      (when (oddp current-exponent)
        (setf result (mod (* result current-base) modulo-number)))
      (setf current-base (mod (* current-base current-base) modulo-number))
      (setf current-exponent (ash current-exponent -1)))
    result))

(defun solve ()
  "Calculates (A(10^9) + B(10^9)) mod 77777777."
  (let* ((modulo-number 77777777)
         (target-n 1000000000)
         (max-k 136)
         (binomial-coeffs (make-array (list (1+ max-k) (1+ max-k)) :initial-element 0))
         (c-coefficients (make-array (1+ max-k) :initial-element 0)))
    
    (format t "debug: pre-computing binomial coefficients up to ~A...~%" max-k)
    (iterate (for i from 0 to max-k)
      (setf (aref binomial-coeffs i 0) 1)
      (iterate (for j from 1 to i)
        (setf (aref binomial-coeffs i j)
              (mod (+ (aref binomial-coeffs (1- i) (1- j))
                      (aref binomial-coeffs (1- i) j))
                   modulo-number))))
                   
    (format t "debug: pre-computing c_i constants...~%")
    (iterate (for i from 0 to max-k)
      (let ((sum 0))
        (iterate (for k from i to max-k)
          (let ((term (aref binomial-coeffs k i)))
            (if (evenp (- k i))
                (incf sum term)
                (decf sum term))))
        (setf (aref c-coefficients i) (mod sum modulo-number))))
        
    (format t "debug: calculating final answer truncating at ~A terms...~%" max-k)
    (let ((final-answer 0)
          (current-p 1))
      (iterate (for j from 0 to max-k)
        
        ;; Calculate F_{n-j} using the mathematically shortened polynomial
        (let ((f-value 0))
          (iterate (for i from 1 to max-k) ;; i=0 yields 0 for any positive exponent
            (let ((term (mod (* (aref c-coefficients i)
                                (power-mod i (- target-n j) modulo-number))
                             modulo-number)))
              (setf f-value (mod (+ f-value term) modulo-number))))
              
          ;; Add (- P_j * F_{n-j}) to final answer
          (let ((term (mod (* current-p f-value) modulo-number)))
            (setf final-answer (mod (+ final-answer term) modulo-number)))
            
          ;; Update current-p = P(n, j+1) for the next iteration
          (setf current-p (mod (* current-p (- target-n j)) modulo-number))))
          
      ;; Result is mathematically defined as - \sum P_j F_{n-j}
      (mod (- modulo-number final-answer) modulo-number))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
debug: pre-computing binomial coefficients up to 136...
debug: pre-computing c_i constants...
debug: calculating final answer truncating at 136 terms...

User time    =        0.042
System time  =        0.003
Elapsed time =        0.022
Allocation   = 189544 bytes
827 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 15955822
:ok