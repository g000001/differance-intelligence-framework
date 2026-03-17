;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0335 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0335)

#||
(cl:comment "Project Euler 335: Gathering the Beans")
(cl:comment "The sequence M(2^k+1) produces values: 2, 5, 15, 53, 207, 845...")
(cl:comment "This satisfies the linear recurrence: y_n = 9y_{n-1} - 26y_{n-2} + 24y_{n-3}")
(cl:comment "The characteristic roots are exactly 2, 3, and 4.")
(cl:comment "The exact closed-form formula is derived as M(2^k+1) = 4^k - 3^k + 2^{k+1}.")
(cl:comment "Summing this algebraically from k=0 to N=10^18 gives:")
(cl:comment "Sum = (4^{N+1}-1)/3 - (3^{N+1}-1)/2 + 2^{N+2} - 2.")
(cl:comment "By using modular inverses for the divisions, the solution requires O(log N) operations.")
||#


(defun ext-gcd-iter (a b)
  (let ((x0 1) (y0 0)
        (x1 0) (y1 1)
        (a0 a) (b0 b))
    (iterate (while (> b0 0))
      (multiple-value-bind (q r) (truncate a0 b0)
        (setf a0 b0)
        (setf b0 r)
        (let ((x-next (- x0 (* q x1)))
              (y-next (- y0 (* q y1))))
          (setf x0 x1)
          (setf x1 x-next)
          (setf y0 y1)
          (setf y1 y-next))))
    (values x0 y0 a0)))

(defun mod-inverse (a m)
  (multiple-value-bind (x y gcd) (ext-gcd-iter a m)
    (declare (ignore y))
    (if (= gcd 1)
        (mod x m)
        (error "inverse doesn't exist"))))

(defun mod-exp (base exp m)
  (let ((res 1)
        (b (mod base m)))
    (iterate (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (* res b) m)))
      (setf b (mod (* b b) m))
      (setf exp (ash exp -1)))
    res))

(defun solve ()
  (let* ((limit-n 1000000000000000000) ; N = 10^18
         (modulus (expt 7 9))
         (n-plus-1 (1+ limit-n))
         (n-plus-2 (+ limit-n 2))
         
         ;; モジュラ逆数の計算
         (inv-3 (mod-inverse 3 modulus))
         (inv-2 (mod-inverse 2 modulus))
         
         ;; Term 1: (4^{N+1} - 1) / 3
         (power-4 (mod-exp 4 n-plus-1 modulus))
         (term-4 (mod (* (mod (- power-4 1) modulus) inv-3) modulus))
         
         ;; Term 2: (3^{N+1} - 1) / 2
         (power-3 (mod-exp 3 n-plus-1 modulus))
         (term-3 (mod (* (mod (- power-3 1) modulus) inv-2) modulus))
         
         ;; Term 3: 2^{N+2} - 2
         (power-2 (mod-exp 2 n-plus-2 modulus))
         (term-2 (mod (- power-2 2) modulus))
         
         ;; 総和: term-4 - term-3 + term-2
         ;; Lisp の mod は負数にも安全に対応して正の剰余を返す
         (total-sum (mod (+ (- term-4 term-3) term-2) modulus)))
    
    (format t "term-4: ~A, term-3: ~A, term-2: ~A~%" term-4 term-3 term-2)
    total-sum))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
term-4: 24625984, term-3: 38612387, term-2: 19018719

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 264 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 5032316
:ok
