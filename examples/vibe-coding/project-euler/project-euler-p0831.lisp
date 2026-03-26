;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0831 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0831)

#||
(cl-text
  (declare (vocab project-euler-831-ontology))
  (forall (m)
    (= (g m)
       (coefficient
         (multiply
           (expt (+ 1 z) 5)
           (expt (/ (- (expt (+ 1 z) 7) 1) z) m))
         (z 5))))
  (assert (= target-m 142857))
  (assert (find-leading-digits-in-base (g target-m) 7 10))
)
||#


(defun multiply-polynomials-mod-z6 (polynomial-alpha polynomial-beta)
  "Multiplies two polynomials of degree up to 5, discarding terms of z^6 and higher."
  (let ((result-polynomial (make-array 6 :initial-element 0)))
    (iterate (for index-i from 0 below 6)
      (iterate (for index-j from 0 below (- 6 index-i))
        (incf (aref result-polynomial (+ index-i index-j))
              (* (aref polynomial-alpha index-i) (aref polynomial-beta index-j)))))
    result-polynomial))

(defun power-polynomial-mod-z6 (base-polynomial target-exponent)
  "Raises a polynomial to a target exponent modulo z^6 using binary exponentiation."
  (let ((result-polynomial (make-array 6 :initial-element 0))
        (current-multiplier (copy-seq base-polynomial))
        (current-exponent target-exponent))
    (setf (aref result-polynomial 0) 1)
    (iterate (while (> current-exponent 0))
      (when (oddp current-exponent)
        (setf result-polynomial (multiply-polynomials-mod-z6 result-polynomial current-multiplier)))
      (setf current-multiplier (multiply-polynomials-mod-z6 current-multiplier current-multiplier))
      (setf current-exponent (ash current-exponent -1)))
    result-polynomial))

(defun calculate-exact-g-function (m-value)
  "Calculates the exact integer value of g(m) using the z^5 coefficient extraction."
  ;; U(z) = ((1+z)^7 - 1) / z = 7 + 21z + 35z^2 + 35z^3 + 21z^4 + 7z^5  (mod z^6)
  (let* ((u-polynomial (make-array 6 :initial-contents '(7 21 35 35 21 7)))
         ;; A(z) = (1+z)^5 = 1 + 5z + 10z^2 + 10z^3 + 5z^4 + z^5 (mod z^6)
         (a-polynomial (make-array 6 :initial-contents '(1 5 10 10 5 1)))
         (u-power-polynomial (power-polynomial-mod-z6 u-polynomial m-value))
         (final-polynomial (multiply-polynomials-mod-z6 a-polynomial u-power-polynomial)))
    (aref final-polynomial 5)))

(defun extract-first-ten-digits-base-7 (massive-integer)
  "Finds the first 10 digits of a massive integer when written in base 7, avoiding full string allocation."
  ;; Estimate the base-7 magnitude to avoid O(N) division loops on 400,000-bit integers.
  (let* ((log2-estimate (1- (integer-length massive-integer)))
         (log7-estimate (floor log2-estimate (log 7.0d0 2.0d0)))
         (power-k (max 0 (- log7-estimate 2)))
         (current-power-of-7 (expt 7 power-k)))
    
    ;; Refine power-k until 7^power-k <= massive-integer < 7^(power-k+1)
    (iterate (while (<= (* current-power-of-7 7) massive-integer))
      (incf power-k)
      (setf current-power-of-7 (* current-power-of-7 7)))
    
    ;; We want exactly the top 10 digits, which means we shift right by (power-k - 9)
    (let* ((shift-amount (- power-k 9))
           (top-ten-value (if (> shift-amount 0)
                              (floor massive-integer (expt 7 shift-amount))
                              (* massive-integer (expt 7 (- shift-amount))))))
      (format nil "~7R" top-ten-value))))

(defun solve ()
  (format t "Starting mathematical reduction for Project Euler 831...~%")
  (let* ((target-m 142857)
         (exact-g-value (calculate-exact-g-function target-m)))
    (format t "-> g(~D) computed exactly (Size: ~D bits).~%" target-m (integer-length exact-g-value))
    (let ((result-string (extract-first-ten-digits-base-7 exact-g-value)))
      (format t "-> The first 10 digits in base 7 are: ~A~%" result-string)
      result-string)))


#+| Do it | (solve )