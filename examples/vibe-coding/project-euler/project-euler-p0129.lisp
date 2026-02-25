
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0129 (:use cl))
(in-package #:project-euler-0129)

;;; ----------------------------------------------------------------------------
;;; SKDT – Dual Sunyata Structures and Emergent Category Theory
;;; ----------------------------------------------------------------------------
;;; This problem involves finding the least 'n' for which A(n) exceeds a large
;;; target. A(n) is defined as the least 'k' such that R(k) is divisible by 'n'.
;;; R(k) is a repunit of length k, which can be expressed as (10^k - 1) / 9.
;;;
;;; The condition "R(k) is divisible by n" means (10^k - 1) / 9 = M * n for some integer M.
;;; This is equivalent to 10^k - 1 = 9 * M * n, which implies 10^k ≡ 1 (mod 9n).
;;; Therefore, A(n) is the multiplicative order of 10 modulo 9n, i.e., ord_{9n}(10).
;;; This interpretation is consistent with all examples given in the problem statement,
;;; including the critical test case "The least value of n for which A(n) first exceeds ten is 17".
;;;
;;; The target value for A(n) in this problem is 1,000,000.
;;; A direct brute-force calculation of ord_{9n}(10) for each 'n' would be too slow
;;; (complexity O(n * A(n))). This initial naive approach is a "Non-Middle Fallacy (NMF)"
;;; or "世俗への執着" as it clings to direct simulation without seeking deeper mathematical
;;; structure.
;;;
;;; To solve this efficiently, we must perform an "ACX Jump (跳躍 ρ)" to a more optimized
;;; algorithm. The multiplicative order 'k' (A(n)) must divide Euler's totient function phi(9n).
;;; Thus, A(n) <= phi(9n).
;;; We also know that phi(X) <= X - 1. So, A(n) <= 9n - 1.
;;; If A(n) > 1,000,000, then 9n - 1 > 1,000,000, which implies n > (1,000,000 + 1) / 9 ≈ 111,111.
;;; A tighter bound can be derived considering the properties of phi(9n):
;;; - If gcd(n, 3) = 1, then phi(9n) = phi(9) * phi(n) = 6 * phi(n) <= 6(n-1).
;;; - If n is a multiple of 3 but not 9 (n=3m), then phi(9n) = phi(27m) = 18 * phi(m) <= 18(m-1) = 6n - 18.
;;; - If n is a multiple of 9 (n=9m), then phi(9n) = phi(81m) = 54 * phi(m) <= 54(m-1) = 6n - 54.
;;; In all cases, A(n) <= 6n - C for some small C.
;;; If A(n) > 1,000,000, then 6n > 1,000,000, implying n > 1,000,000 / 6 ≈ 166,666.66.
;;; Thus, we can start searching for 'n' from approximately 166,667.
;;;
;;; The optimized A(n) calculation ("Continuation (継続 κ)") leverages number theory:
;;; 1. Calculate `M = 9n`.
;;; 2. Calculate `phi(M)` using Euler's totient function (complexity O(sqrt(M))).
;;; 3. Find the unique prime factors of `phi(M)` (complexity O(sqrt(phi(M)))).
;;; 4. Iteratively test `10^(phi(M)/p) mod M` for each prime factor `p` to reduce `phi(M)` to the true order (complexity O(num_factors * log(phi(M)))).
;;;
;;; The overall complexity will be approximately O(N_max * (sqrt(9 * N_max) + log(phi(9 * N_max)))),
;;; where N_max is the maximum 'n' we expect to check. If N_max is around 1.7 * 10^5,
;;; this translates to roughly (1.7 * 10^5) * (3 * sqrt(1.7 * 10^5)) ≈ 1.7 * 10^5 * 1200 ≈ 2 * 10^8 operations.
;;; This computational cost is acceptable within typical Project Euler time limits (a few seconds).
;;;
;;; The Lisp code that "現成 (manifests)" these mathematical structures into executable form
;;; embodies the "中道 (Middle Way)" by balancing theoretical efficiency with practical computation.
;;; ----------------------------------------------------------------------------

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun mod-expt (base exp mod)
  "Calculates (base^exp) mod mod efficiently using modular exponentiation."
  (declare (type (integer 0 *) base exp mod))
  (loop with result of-type (integer 0 *) = 1
        for e of-type (integer 0 *) = exp then (ash e -1)
        for b of-type (integer 0 *) = (mod base mod) then (mod (* b b) mod)
        while (> e 0)
        when (oddp e) do (setf result (mod (* result b) mod))
        finally (return result)))

(defun phi-correct (n)
  "Calculates Euler's totient function phi(n)."
  (declare (type (integer 0 *) n))
  (when (= n 0) (return-from phi-correct 0))
  (let ((result n)
        (temp n))
    (declare (type (integer 0 *) result temp))
    (when (evenp temp)
      (setf result (the (integer 0 *) (/ result 2)))
      (loop while (evenp temp) do (setf temp (the (integer 0 *) (/ temp 2)))))
    (loop for p from 3 by 2
          while (<= (* p p) temp)
          do (when (zerop (mod temp p))
               (setf result (the (integer 0 *) (- result (/ result p))))
               (loop while (zerop (mod temp p)) do (setf temp (the (integer 0 *) (/ temp p)))))
          finally
            (when (> temp 1)
              (setf result (the (integer 0 *) (- result (/ result temp))))))
    result))

(defun unique-prime-factors (n)
  "Returns a list of unique prime factors of n."
  (declare (type (integer 0 *) n))
  (let ((factors '())
        (d 2))
    (declare (type (integer 0 *) d))
    (loop while (and (<= (* d d) n) (> n 1))
          do (when (zerop (mod n d))
               (push d factors)
               (loop while (zerop (mod n d))
                     do (setf n (the (integer 0 *) (/ n d)))))
             (setf d (if (= d 2) 3 (+ d 2))))
    (when (> n 1)
      (push n factors))
    (nreverse factors))) ; Reverse to get factors in ascending order

(defun calculate-A (n)
  "Calculates A(n), the least k for which R(k) is divisible by n.
   A(n) = ord_{9n}(10)."
  (declare (type (integer 1 *) n))
  (let* ((m (* 9 n))
         (order (phi-correct m)))
    (declare (type (integer 0 *) m order))
    (when (zerop order) ; Should not happen for n >= 1, but for safety
      (return-from calculate-A 0))
    (let ((factors (unique-prime-factors order)))
      (declare (list factors))
      (loop for p in factors
            do (loop while (and (zerop (mod order p))
                                (= (mod-expt 10 (the (integer 0 *) (/ order p)) m) 1))
                     do (setf order (the (integer 0 *) (/ order p)))))
      order)))

(defun solve ()
  "Finds the least value of n for which A(n) first exceeds one-million."
  (let ((target 1000000))
    ;; As derived in the SKDT comments, n must be at least 166,667 to have A(n) > 1,000,000.
    ;; We iterate n starting from 166,668 (the smallest integer > 166,667).
    ;; The problem specifies gcd(n, 10) = 1, so n must not be divisible by 2 or 5.
    (loop for n from 166668
          do (when (and (not (zerop (mod n 2))) ; n must be odd
                        (not (zerop (mod n 5)))) ; n must not be a multiple of 5
               (let ((a-n (calculate-A n)))
                 (when (> a-n target)
                   (return-from solve n)))))))
#+| Do it | (solve )
;→ 1000023
