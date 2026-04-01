;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0792 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0792)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun compute-nu2-of-C (target-n)
  "Computes nu_2(C_n) exactly using the integer recurrence relation.
   2n * C_n = (7n-4) * C_{n-1} + (4n-2) * C_{n-2}.
   WARNING: This is strictly an O(N) evaluation. For target-n up to 10^12, 
   this will computationally exceed the 60-second rule."
  (declare (type integer target-n))
  (if (= target-n 0)
      0
      (if (= target-n 1)
          0
          (let ((c-prev 1)
                (c-curr 1))
            (do ((k 2 (1+ k)))
                ((> k target-n))
              (let* ((term1 (* (- (* 7 k) 4) c-curr))
                     (term2 (* (- (* 4 k) 2) c-prev))
                     (next-val (/ (+ term1 term2) (* 2 k))))
                (setf c-prev c-curr)
                (setf c-curr next-val)))
            ;; Extract the 2-adic valuation (trailing zeros)
            (let ((v 0)
                  (temp (abs c-curr)))
              (do () ((oddp temp) v)
                (incf v)
                (setf temp (ash temp -1))))))))

(defun compute-u-exact (n)
  "Computes u(n) = nu_2(3S(n)+4) based on the exact structural reduction."
  (declare (type integer n))
  (+ n 2 (compute-nu2-of-C n)))

(defun solve-euler-0792 (&optional (limit-n 10000))
  (declare (type fixnum limit-n))
  (let ((total-u 0))
    ;; We use standard iteration here to print progress, as the sheer computational 
    ;; weight makes silent series processing appear as a frozen state.
    (do ((n 1 (1+ n)))
        ((> n limit-n))
      (let* ((n-cubed (* n n n))
             (u-val (compute-u-exact n-cubed)))
        (incf total-u u-val)
        (when (zerop (mod n 5))
          (format t "Progress: n=~A, n^3=~A, u=~A~%" n n-cubed u-val))))
    total-u))

(defun solve ()
  (format t "--- Mathematical Grounding Validation ---~%")
  (format t "Testing u(4)... Expected: 7, Got: ~A~%" (compute-u-exact 4))
  (format t "Testing u(20)... Expected: 24, Got: ~A~%" (compute-u-exact 20))
  (format t "-----------------------------------------~%")
  (format t "Solving for U(10^4)...~%")
  (format t "Notice: Expected to exceed 60 seconds due to missing O(log n) bit-pattern jump.~%")
  (let ((ans (solve-euler-0792 10000)))
    (format t "Answer: ~A~%" ans)
    ans))



 
