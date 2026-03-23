;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0410 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0410)

#||
Project Euler 410: Circle and tangent line

Let the circle be x^2 + y^2 = r^2.
Points P(a, b) and Q(-a, c) form a tangent line to the circle.
By applying the distance from the origin to the line y = mx + k, we get:
4 r^2 a^2 = a^2 (b+c)^2 - r^2 (b-c)^2
Let g = gcd(r, a). r = g R, a = g A.
Then the number of valid pairs (b, c) for a given (r, a) depends entirely on g and the parity of R and A.
After dimensional collapse using Dirichlet convolution and Möbius inversion, we find:
F(N, X) = 2 \sum_{k=1}^N [ A(k) \lfloor N/k \rfloor \lfloor X/k \rfloor + B(k) \lfloor (N/k+1)/2 \rfloor \lfloor (X/k+1)/2 \rfloor ]
where A(k) is a multiplicative function with A(p^a) = 2 for odd p, and A(2^a) = 2 (a>=2), 0 (a=1).
And B(k) = 2 A(m) for k = 2^a m (m odd, a>=1), 0 otherwise.
By grouping terms for each odd part m, we can completely eliminate B(k) and only evaluate A(m) for odd m.
A(m) for odd m is exactly 2^{\omega(m)}, which can be computed via a highly efficient sieve over odd numbers only.
Finally, F(N, X) = F(X, N), so the target F(10^8, 10^9) + F(10^9, 10^8) = 2 F(10^8, 10^9).
||#

(defconstant $max-r #.(expt 10 8))
(defconstant $max-x #.(expt 10 9))
(defconstant $half-max-r #.(floor (expt 10 8) 2))

(define-modify-macro ashf (val) ash)

(defun solve ()
  (let* ((number-of-odds $half-max-r)
         (limit-r $max-r)
         (limit-x $max-x)
         ;; Array to store 2^{\omega(m)} for odd m. Index i corresponds to odd number 2i+1.
         (omega-powers (make-array number-of-odds :element-type '(unsigned-byte 8) :initial-element 1))
         (total-quadruplets 0))
    
    ;; Sieve to compute 2^{\omega(m)}. Only iterates over odd numbers to slash memory and time by half.
    (iterate (for i from 1 below number-of-odds)
      (when (= (aref omega-powers i) 1)
        ;; 2i+1 is a prime number. Multiply all its odd multiples by 2.
        (let ((prime-p (1+ (* 2 i))))
          (iterate (for j from i below number-of-odds by prime-p)
            (setf (aref omega-powers j) (ash (aref omega-powers j) 1))))))
            
    ;; Accumulate the total quadruplets using the grouped odd parts
    (iterate (for i from 0 below number-of-odds)
      (let* ((odd-m (1+ (* 2 i)))
             (a-value (aref omega-powers i))
             (current-r-floor (floor limit-r odd-m))
             (current-x-floor (floor limit-x odd-m))
             (m-contribution (* current-r-floor current-x-floor)))
             
        ;; Shift right by 1 is equivalent to floor(val / 2)
        (ashf current-r-floor -1)
        (ashf current-x-floor -1)
        
        (when (> current-r-floor 0)
          ;; (ash (1+ val) -1) is equivalent to floor((val + 1) / 2)
          (incf m-contribution (* 2 (ash (1+ current-r-floor) -1) (ash (1+ current-x-floor) -1)))
          (ashf current-r-floor -1)
          (ashf current-x-floor -1)
          
          (iterate (while (> current-r-floor 0))
            (incf m-contribution (* 2 (+ (* current-r-floor current-x-floor)
                                         (* (ash (1+ current-r-floor) -1) (ash (1+ current-x-floor) -1)))))
            (ashf current-r-floor -1)
            (ashf current-x-floor -1)))
            
        (incf total-quadruplets (* a-value m-contribution))))
        
    ;; Multiply by 4 because F(R, X) + F(X, R) = 2 * F(R, X) = 4 * \sum (A(m) * M(m))
    (* 4 total-quadruplets)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        9.340
System time  =        0.131
Elapsed time =        9.332
Allocation   = 4345285448 bytes
16282 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 799999783589946560
:ok