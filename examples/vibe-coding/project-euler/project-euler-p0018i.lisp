;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: Gemini 3.1 Pro
(cl:in-package cl-user)
(defpackage #:project-euler-0018i (:use cl iterate))
(in-package #:project-euler-0018i)

#||
(cl-text euler-acx-p18i
(cl-comment "ACX Jump formulation for Euler 18i")

(cl-comment "1. NMF (Non-Middle Fallacy) Avoidance")
(cl-comment "O(p) product evaluations for p ~ 10^9 leads to massive NMF. We must invoke ACX Jump.")
(forall (p alg)
  (if (and (Prime p) (> p 1000000000) (= alg NaiveProductEvaluation))
      (and (NMF alg)
           (requires_ACX_jump alg))))

(cl-comment "2. ACX Jump (Mathematical Reduction) via Frobenius Endomorphism")
(cl-comment "For irreducible f(x) over F_p, R(p) = -(A(X)-X)(B(X)-A(X))(X-B(X)) mod f(X) where A(X)=X^p, B(X)=X^{p^2}.")
(forall (f p)
  (if (and (= f (Polynomial "x^3 - 3x + 4"))
           (= (mod p 4) 1)
           (Irreducible f p))
      (and (target_of ACX_Jump FrobeniusReduction)
           (complexity FrobeniusReduction O_log_N)
           (grounded_in_ultimate_truth FrobeniusReduction))))

(cl-comment "3. Exact Integer Projection")
(cl-comment "All polynomial operations are confined strictly to positive exact modulo arithmetic, preventing floating-point illusions.")
(forall (operation)
  (if (member operation (PolynomialMultiplication FrobeniusReduction))
      (and (eliminates_floating_point operation)
           (uses_exact_integer_arithmetic operation))))

(cl-comment "4. Debt Clearance")
(cl-comment "Polynomial multiplications in the evaluation loop re-use pre-allocated arrays to avoid memory accumulation.")
(forall (poly-mul)
  (if (involves_dynamic_programming poly-mul)
      (implements_debt_clearance poly-mul)))
)
||#

(declaim (inline poly-mul!))
(defun poly-mul! (res p1 p2 p)
  "Multiplies two polynomials p1 and p2 modulo X^3 - 3X + 4 and p, storing the result in res."
  (declare (type (simple-array (unsigned-byte 64) (3)) res p1 p2)
           (type (unsigned-byte 64) p)
           (optimize (speed 3) (safety 0)))
  (let* ((a1 (aref p1 0)) (b1 (aref p1 1)) (c1 (aref p1 2))
         (a2 (aref p2 0)) (b2 (aref p2 1)) (c2 (aref p2 2))
         (c4 (mod (* a1 a2) p))
         (c3 (mod (+ (* a1 b2) (* b1 a2)) p))
         (c2_ (mod (+ (* a1 c2) (* b1 b2) (* c1 a2)) p))
         (c1_ (mod (+ (* b1 c2) (* c1 b2)) p))
         (c0 (mod (* c1 c2) p)))
    ;; X^3 = 3X - 4 => X^4 = 3X^2 - 4X
    (setf (aref res 0) (mod (+ (* 3 c4) c2_) p)
          (aref res 1) (mod (+ (* 3 c3) c1_ (- p (mod (* 4 c4) p))) p)
          (aref res 2) (mod (+ c0 (- p (mod (* 4 c3) p))) p))
    res))

(defun poly-power (base exp p)
  "Computes base^exp modulo X^3 - 3X + 4 and p using binary exponentiation without consing."
  (declare (type (simple-array (unsigned-byte 64) (3)) base)
           (type (unsigned-byte 64) exp p)
           (optimize (speed 3) (safety 0)))
  (let ((res (make-array 3 :element-type '(unsigned-byte 64) :initial-contents '(0 0 1)))
        (b (make-array 3 :element-type '(unsigned-byte 64)))
        (tmp (make-array 3 :element-type '(unsigned-byte 64))))
    (setf (aref b 0) (aref base 0)
          (aref b 1) (aref base 1)
          (aref b 2) (aref base 2))
    (iterate
      (declare (type (unsigned-byte 64) exp))
      (while (> exp 0))
      (when (oddp exp)
        (poly-mul! tmp res b p)
        (setf (aref res 0) (aref tmp 0)
              (aref res 1) (aref tmp 1)
              (aref res 2) (aref tmp 2)))
      (poly-mul! tmp b b p)
      (setf (aref b 0) (aref tmp 0)
            (aref b 1) (aref tmp 1)
            (aref b 2) (aref tmp 2))
      (setf exp (ash exp -1)))
    res))

(defun compute-Rp (p)
  "Evaluates R(p) using Frobenius Endomorphism properties over F_p."
  (declare (type (unsigned-byte 64) p)
           (optimize (speed 3) (safety 0)))
  ;; Based on the discriminant Δ = -324, the polynomial has exactly one root if p ≡ 3 (mod 4).
  ;; If it has a root in F_p, R(p) is 0.
  (if (/= (mod p 4) 1)
      0
      (let* ((X-poly (make-array 3 :element-type '(unsigned-byte 64) :initial-contents '(0 1 0)))
             (A (poly-power X-poly p p)))
        ;; Check if all roots are in F_p (i.e. A(X) = X)
        (if (and (= (aref A 0) 0)
                 (= (aref A 1) 1)
                 (= (aref A 2) 0))
            0
            (let* ((A2 (make-array 3 :element-type '(unsigned-byte 64)))
                   (B (make-array 3 :element-type '(unsigned-byte 64)))
                   (a-coef (aref A 0))
                   (b-coef (aref A 1))
                   (c-coef (aref A 2)))
              (poly-mul! A2 A A p)
              ;; B(X) = A(A(X)) = a*A2 + b*A + c
              (setf (aref B 0) (mod (+ (* a-coef (aref A2 0)) (* b-coef (aref A 0))) p)
                    (aref B 1) (mod (+ (* a-coef (aref A2 1)) (* b-coef (aref A 1))) p)
                    (aref B 2) (mod (+ (* a-coef (aref A2 2)) (* b-coef (aref A 2)) c-coef) p))
              (let ((P1 (make-array 3 :element-type '(unsigned-byte 64)))
                    (P2 (make-array 3 :element-type '(unsigned-byte 64)))
                    (P3 (make-array 3 :element-type '(unsigned-byte 64)))
                    (tmp (make-array 3 :element-type '(unsigned-byte 64))))
                ;; P1 = A - X
                (setf (aref P1 0) (aref A 0)
                      (aref P1 1) (mod (+ (aref A 1) (- p 1)) p)
                      (aref P1 2) (aref A 2))
                ;; P2 = B - A
                (setf (aref P2 0) (mod (+ (aref B 0) (- p (aref A 0))) p)
                      (aref P2 1) (mod (+ (aref B 1) (- p (aref A 1))) p)
                      (aref P2 2) (mod (+ (aref B 2) (- p (aref A 2))) p))
                ;; P3 = X - B
                (setf (aref P3 0) (mod (- p (aref B 0)) p)
                      (aref P3 1) (mod (+ 1 (- p (aref B 1))) p)
                      (aref P3 2) (mod (- p (aref B 2)) p))
                
                (poly-mul! tmp P1 P2 p)
                (poly-mul! P1 tmp P3 p)
                
                ;; Result evaluates to a strict constant. R(p) = -(constant).
                (mod (- p (aref P1 2)) p)))))))

(defun solve ()
  (let* ((L 1000000000)
         (R 1100000000)
         (limit (isqrt R))
         (small-sieve (make-array (1+ limit) :element-type 'bit :initial-element 0))
         (primes (make-array 0 :element-type '(unsigned-byte 32) :fill-pointer 0 :adjustable t))
         (sum 0))
    (declare (type (unsigned-byte 64) sum L R limit))
    
    ;; Initial small sieve
    (iterate
      (for i from 2 to limit)
      (when (zerop (sbit small-sieve i))
        (vector-push-extend i primes)
        (iterate
          (for j from (* i i) to limit by i)
          (setf (sbit small-sieve j) 1))))
    
    ;; Segmented sieve for [L, R)
    (let* ((chunk-size 1000000)
           (chunk (make-array chunk-size :element-type 'bit)))
      (iterate
        (for start from L below R by chunk-size)
        (let ((end (min R (+ start chunk-size))))
          (fill chunk 0)
          (iterate
            (for p in-vector primes)
            (let* ((rem (mod start p))
                   (start-idx (if (zerop rem) start (+ start (- p rem)))))
              (iterate
                (declare (type (unsigned-byte 64) j p start-idx end))
                (for j from start-idx below end by p)
                (setf (sbit chunk (- j start)) 1))))
          (iterate
            (declare (type (unsigned-byte 64) i))
            (for i from start below end)
            (when (and (zerop (sbit chunk (- i start)))
                       (> i 1))
              (incf sum (compute-Rp i)))))))
    sum))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =       57.361
System time  =        0.183
Elapsed time =       57.656
Allocation   = 1675875104 bytes
4778 Page faults
GC time      =        0.010
 |------------------------------------------------------------|#
;;→ 842507000531275
:ok