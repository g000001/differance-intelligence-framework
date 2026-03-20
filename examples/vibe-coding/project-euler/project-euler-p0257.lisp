;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0257 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0257)

#||
(clif-logic
  (formal-problem "Project Euler 257: Angular Bisectors")
  (invariants
    (area-ratio-integral-condition
      (iff (integral-ratio-p a b c)
           (or (= (* a (+ a b c)) (* b c))        ; k=2
               (= (* a (+ a b c)) (* 2 b c))      ; k=3
               (= a b c))))                       ; k=4
    (parametrization
      (and
        (group-1-xy=2a^2 (exists (m n) (= (* x y) (* 2 a a))))
        (group-2-xy=3a^2 (exists (m n) (= (* x y) (* 3 a a)))))))
  (optimizations
    (dimension-reduction "O(L^2) naive search reduced to O(L^(1/2)) by parameterizing the Diophantine equations.")
    (float-elimination "Using isqrt for exact integer bounds on n to prevent any precision loss at high limits.")
    (garbage-collection "Zero allocation in inner loops. Fixnum arithmetic strictly bounded within 61-bit registers.")))
||#

(declaim (inline min-n-1a min-n-1b min-n-2a min-n-2b))
(defun min-n-1a (m) (1+ (isqrt (* 2 m m))))
(defun min-n-1b (m)
  (let ((n (isqrt (truncate (* m m) 2))))
    (if (<= (* 2 n n) (* m m)) (1+ n) n)))
(defun min-n-2a (m) (1+ (isqrt (* 3 m m))))
(defun min-n-2b (m)
  (let ((n (isqrt (truncate (* m m) 3))))
    (if (<= (* 3 n n) (* m m)) (1+ n) n)))

(defun solve ()
  "Calculates the number of triangles with perimeter <= 100,000,000 
   having an integral area ratio."
  (let ((limit 100000000)
        (ans 0))
    
    ;; [1] Equilateral triangles (k = 4)
    (incf ans (truncate limit 3))
    
    ;; [2] Group 1-A (k = 2)
    ;; x = 2m^2, y = n^2, n is odd, gcd(m,n)=1, sqrt(2)m < n < 2m
    (iterate (for m from 1)
      (let* ((base-min-n (min-n-1a m))
             (min-n (if (evenp base-min-n) (1+ base-min-n) base-min-n))
             (max-n (1- (* 2 m))))
        (when (> (* (+ (* 2 m) base-min-n) (+ m base-min-n)) limit)
          (finish))
        (iterate (for n from min-n to max-n by 2)
          (let ((p (* (+ (* 2 m) n) (+ m n))))
            (when (> p limit) (finish))
            (when (= 1 (gcd m n))
              (incf ans (truncate limit p)))))))
              
    ;; [3] Group 1-B (k = 2)
    ;; x = m^2, y = 2n^2, m is odd, gcd(m,n)=1, n < m < sqrt(2)n
    (iterate (for m from 3 by 2)
      (let* ((min-n (min-n-1b m))
             (max-n (1- m)))
        (when (> (* (+ m min-n) (+ m (* 2 min-n))) limit)
          (finish))
        (iterate (for n from min-n to max-n)
          (let ((p (* (+ m n) (+ m (* 2 n)))))
            (when (> p limit) (finish))
            (when (= 1 (gcd m n))
              (incf ans (truncate limit p)))))))
              
    ;; [4] Group 2-A (k = 3)
    ;; x = 3m^2, y = n^2, m,n are odd, gcd(m,n)=1, n != 0 mod 3, sqrt(3)m < n < 3m
    (iterate (for m from 1 by 2)
      (let* ((base-min-n (min-n-2a m))
             (min-n (if (evenp base-min-n) (1+ base-min-n) base-min-n))
             (max-n (1- (* 3 m))))
        (when (> (truncate (* (+ (* 3 m) base-min-n) (+ m base-min-n)) 2) limit)
          (finish))
        (iterate (for n from min-n to max-n by 2)
          (let ((p (truncate (* (+ (* 3 m) n) (+ m n)) 2)))
            (when (> p limit) (finish))
            (when (and (not (zerop (mod n 3)))
                       (= 1 (gcd m n)))
              (incf ans (truncate limit p)))))))
              
    ;; [5] Group 2-B (k = 3)
    ;; x = m^2, y = 3n^2, m,n are odd, gcd(m,n)=1, m != 0 mod 3, n < m < sqrt(3)n
    (iterate (for m from 3 by 2)
      (when (zerop (mod m 3)) (next-iteration))
      (let* ((base-min-n (min-n-2b m))
             (min-n (if (evenp base-min-n) (1+ base-min-n) base-min-n))
             (max-n (1- m)))
        (when (> (truncate (* (+ m base-min-n) (+ m (* 3 base-min-n))) 2) limit)
          (finish))
        (iterate (for n from min-n to max-n by 2)
          (let ((p (truncate (* (+ m n) (+ m (* 3 n))) 2)))
            (when (> p limit) (finish))
            (when (= 1 (gcd m n))
              (incf ans (truncate limit p)))))))
              
    ;; [6] Group 3-A (k = 3, even parameter variation)
    ;; x = 3m^2, y = n^2, m != n mod 2, gcd(m,n)=1, n != 0 mod 3, sqrt(3)m < n < 3m
    (iterate (for m from 1)
      (let* ((base-min-n (min-n-2a m))
             (min-n (if (= (mod base-min-n 2) (mod m 2)) (1+ base-min-n) base-min-n))
             (max-n (1- (* 3 m))))
        (when (> (* (+ (* 3 m) base-min-n) (+ m base-min-n)) limit)
          (finish))
        (iterate (for n from min-n to max-n by 2)
          (let ((p (* (+ (* 3 m) n) (+ m n))))
            (when (> p limit) (finish))
            (when (and (not (zerop (mod n 3)))
                       (= 1 (gcd m n)))
              (incf ans (truncate limit p)))))))
              
    ;; [7] Group 3-B (k = 3, even parameter variation)
    ;; x = m^2, y = 3n^2, m != n mod 2, gcd(m,n)=1, m != 0 mod 3, n < m < sqrt(3)n
    (iterate (for m from 2)
      (when (zerop (mod m 3)) (next-iteration))
      (let* ((base-min-n (min-n-2b m))
             (min-n (if (= (mod base-min-n 2) (mod m 2)) (1+ base-min-n) base-min-n))
             (max-n (1- m)))
        (when (> (* (+ m base-min-n) (+ m (* 3 base-min-n))) limit)
          (finish))
        (iterate (for n from min-n to max-n by 2)
          (let ((p (* (+ m n) (+ m (* 3 n)))))
            (when (> p limit) (finish))
            (when (= 1 (gcd m n))
              (incf ans (truncate limit p)))))))
              
    ;; Final Answer
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        1.248
System time  =        0.023
Elapsed time =        1.216
Allocation   = 224880 bytes
3712 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 139012411
:ok