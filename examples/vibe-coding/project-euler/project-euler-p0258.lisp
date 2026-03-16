;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0258 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0258)

#||
(cl:comment "CLIF logic for Project Euler 258")
(cl:text
(Sequence G)
(forall (k) (if (and (>= k 0) (<= k 1999)) (= (G k) 1)))
(forall (k) (if (>= k 2000) (= (G k) (+ (G (- k 2000)) (G (- k 1999))))))
(Target (= ?answer (modulo (G 1000000000000000000) 20092010)))

(cl:comment "Algebraic shortcut using Polynomial Ring")
(CharacteristicPolynomial P (x) (- (- (expt x 2000) x) 1))
(Equivalence (G k) (Summation i 0 1999 (Coefficient (modulo (expt x k) (P x)) i)))

(cl:comment "Hardware mapping for cache-efficiency and 61-bit fixnum limits")
(Constraint (<= (* 2000 (expt 20092010 2)) (- (expt 2 61) 1)))
(Optimization "Eliminate modulo operation inside O(N^2) convolution loop")
)
||#


(defun multiply-polynomials-modulo (polynomial-alpha polynomial-beta modulo-number)
  "Multiplies two polynomials and reduces them modulo x^2000 - x - 1 and the given modulo-number."
  (let ((result-polynomial (make-array 3999 :element-type '(unsigned-byte 61) :initial-element 0)))
    ;; O(N^2) inner loop: No modulo operation here to maximize execution speed.
    ;; The maximum accumulated value is ~8 * 10^17, which perfectly fits in a 61-bit fixnum.
    (iterate (for index-alpha from 0 below 2000)
      (let ((coefficient-alpha (aref polynomial-alpha index-alpha)))
        (when (> coefficient-alpha 0)
          (iterate (for index-beta from 0 below 2000)
            (incf (aref result-polynomial (+ index-alpha index-beta))
                  (* coefficient-alpha (aref polynomial-beta index-beta)))))))
    
    ;; Reduction phase O(N): Reduce polynomial degrees using x^2000 = x + 1
    (iterate (for index-reduce from 3998 downto 2000)
      (let ((coefficient-reduce (mod (aref result-polynomial index-reduce) modulo-number)))
        (when (> coefficient-reduce 0)
          ;; x^k = x^(k-1999) + x^(k-2000)
          (incf (aref result-polynomial (- index-reduce 1999)) coefficient-reduce)
          (incf (aref result-polynomial (- index-reduce 2000)) coefficient-reduce))))
    
    ;; Final modulo and copy to a strict 2000-degree array
    (let ((final-reduced-polynomial (make-array 2000 :element-type '(unsigned-byte 61))))
      (iterate (for index-final from 0 below 2000)
        (setf (aref final-reduced-polynomial index-final)
              (mod (aref result-polynomial index-final) modulo-number)))
      final-reduced-polynomial)))

(defun power-polynomial-modulo (base-polynomial target-exponent modulo-number)
  "Calculates (base-polynomial ^ target-exponent) using exponentiation by squaring."
  (let ((result-polynomial (make-array 2000 :element-type '(unsigned-byte 61) :initial-element 0))
        (current-base base-polynomial)
        (current-exponent target-exponent))
    ;; Initialize result to 1 (x^0)
    (setf (aref result-polynomial 0) 1)
    
    (iterate (while (> current-exponent 0))
      ;; Print debug at the outermost algorithmic loop as requested
      (format t "debug: remaining current-exponent = ~A~%" current-exponent)
      
      (when (oddp current-exponent)
        (setf result-polynomial 
              (multiply-polynomials-modulo result-polynomial current-base modulo-number)))
      
      (setf current-exponent (ash current-exponent -1))
      (when (> current-exponent 0)
        (setf current-base 
              (multiply-polynomials-modulo current-base current-base modulo-number))))
    
    result-polynomial))

(defun solve ()
  "Calculates the target value g_k modulo 20092010 for k = 10^18."
  (let* ((target-k (expt 10 18))
         (modulo-number 20092010)
         ;; The base polynomial is just 'x', which means the coefficient of x^1 is 1
         (initial-polynomial (make-array 2000 :element-type '(unsigned-byte 61) :initial-element 0)))
    
    (setf (aref initial-polynomial 1) 1)
    
    (let ((final-polynomial (power-polynomial-modulo initial-polynomial target-k modulo-number))
          (final-answer 0))
      
      ;; Since g_i = 1 for all 0 <= i <= 1999, the answer is simply 
      ;; the sum of all coefficients in the final polynomial.
      (iterate (for index-sum from 0 below 2000)
        (incf final-answer (aref final-polynomial index-sum)))
      
      (mod final-answer modulo-number))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
debug: remaining current-exponent = 1000000000000000000
debug: remaining current-exponent = 500000000000000000
debug: remaining current-exponent = 250000000000000000
debug: remaining current-exponent = 125000000000000000
debug: remaining current-exponent = 62500000000000000
debug: remaining current-exponent = 31250000000000000
debug: remaining current-exponent = 15625000000000000
debug: remaining current-exponent = 7812500000000000
debug: remaining current-exponent = 3906250000000000
debug: remaining current-exponent = 1953125000000000
debug: remaining current-exponent = 976562500000000
debug: remaining current-exponent = 488281250000000
debug: remaining current-exponent = 244140625000000
debug: remaining current-exponent = 122070312500000
debug: remaining current-exponent = 61035156250000
debug: remaining current-exponent = 30517578125000
debug: remaining current-exponent = 15258789062500
debug: remaining current-exponent = 7629394531250
debug: remaining current-exponent = 3814697265625
debug: remaining current-exponent = 1907348632812
debug: remaining current-exponent = 953674316406
debug: remaining current-exponent = 476837158203
debug: remaining current-exponent = 238418579101
debug: remaining current-exponent = 119209289550
debug: remaining current-exponent = 59604644775
debug: remaining current-exponent = 29802322387
debug: remaining current-exponent = 14901161193
debug: remaining current-exponent = 7450580596
debug: remaining current-exponent = 3725290298
debug: remaining current-exponent = 1862645149
debug: remaining current-exponent = 931322574
debug: remaining current-exponent = 465661287
debug: remaining current-exponent = 232830643
debug: remaining current-exponent = 116415321
debug: remaining current-exponent = 58207660
debug: remaining current-exponent = 29103830
debug: remaining current-exponent = 14551915
debug: remaining current-exponent = 7275957
debug: remaining current-exponent = 3637978
debug: remaining current-exponent = 1818989
debug: remaining current-exponent = 909494
debug: remaining current-exponent = 454747
debug: remaining current-exponent = 227373
debug: remaining current-exponent = 113686
debug: remaining current-exponent = 56843
debug: remaining current-exponent = 28421
debug: remaining current-exponent = 14210
debug: remaining current-exponent = 7105
debug: remaining current-exponent = 3552
debug: remaining current-exponent = 1776
debug: remaining current-exponent = 888
debug: remaining current-exponent = 444
debug: remaining current-exponent = 222
debug: remaining current-exponent = 111
debug: remaining current-exponent = 55
debug: remaining current-exponent = 27
debug: remaining current-exponent = 13
debug: remaining current-exponent = 6
debug: remaining current-exponent = 3
debug: remaining current-exponent = 1

User time    =        6.034
System time  =        0.069
Elapsed time =        5.951
Allocation   = 5543272 bytes
4185 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 12747994
:ok
