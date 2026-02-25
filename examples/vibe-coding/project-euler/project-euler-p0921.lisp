
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0921 
  (:use cl #:iterate))
(in-package #:project-euler-0921)

;;; ==============================================================
;;; SKDT – Dual Sunyata Structures and Emergent Category Theory
;;; --------------------------------------------------------------
;;; Problem 921: Recurrence and Fibonacci Summation
;;; Solution using the Two-Truths Entanglement (二諦随伴) Protocol
;;; ==============================================================

(defun power-mod (a b m)
  "Modular exponentiation: a^b mod m."
  (if (= m 1) (return-from power-mod 0))
  (let ((res 1))
    (setf a (mod a m))
    (iterate (while (> b 0))
             (when (oddp b)
               (setf res (mod (* res a) m)))
             (setf a (mod (* a a) m))
             (setf b (ash b -1)))
    res))

(defun mod-sqrt (n p)
  "Tonelli-Shanks algorithm to find the square root of n modulo p."
  (if (/= (power-mod n (ash (- p 1) -1) p) 1)
      (error "Not a quadratic residue"))
  (let* ((q (- p 1))
         (ss 0))
    ;; Grounding the complexity: p-1 = Q * 2^S
    (iterate (while (evenp q))
             (setf q (ash q -1))
             (incf ss))
    (if (= ss 1)
        (return-from mod-sqrt (power-mod n (ash (+ p 1) -2) p)))
    ;; Search for a non-residue z (Potentiality)
    (let ((z 2))
      (iterate (while (= (power-mod z (ash (- p 1) -1) p) 1))
               (incf z))
      (let ((c (power-mod z q p))
            (t-val (power-mod n q p))
            (r (power-mod n (ash (+ q 1) -1) p))
            (mm ss))
        ;; Iterative refinement (Manifestation)
        (iterate
          (if (= t-val 0) (leave 0))
          (if (= t-val 1) (leave r))
          (let ((i (iterate (for i from 1 below mm)
                            (when (= (power-mod t-val (ash 1 i) p) 1)
                              (return i))
                            (finally (return mm)))))
            (let* ((b (power-mod c (ash 1 (- mm i 1)) p))
                   (b2 (mod (* b b) p)))
              (setf mm i
                    c b2
                    t-val (mod (* t-val b2) p)
                    r (mod (* r b) p)))))))))

(defun solve ()
  "Calculates S(1618034) modulo 398874989."
  (let* ((m 398874989)               ; The Modulus (Conventional Truth)
         (limit 1618034)             ; m = floor(phi * 10^6)
         (p (1- m))                  ; Period for phi^k mod m
         (o 199437492)               ; phi(p) = 2 * (99718747 - 1)
         (r (mod-sqrt 5 m))          ; sqrt(5) mod m
         (inv2 (mod (ash (1+ m) -1) m)) ; 2^-1 mod m
         (phi (mod (* (1+ r) inv2) m))  ; Golden Ratio phi mod m
         (invr (power-mod r (- m 2) m)) ; sqrt(5)^-1 mod m
         (total-sum 0))
    
    ;; We identify the recurrence a_{n+1} = f(a_n) as a coth(5x) transformation.
    ;; a_n = coth(5^n * x_0) where e^(2x_0) = phi^3.
    ;; This leads to a_n = (phi^k + 1)/(phi^k - 1) for k = 3 * 5^n.
    ;; a_n = (F_k * sqrt(5) + 2) / L_k.
    ;; p_n = F_k / 2, q_n = L_k / 2.
    ;; s(n) = (F_k/2)^5 + (L_k/2)^5.
    
    (iterate (for i from 2 to limit)
             ;; Fibonacci sequence F_i mod O (The Middle Way of indices)
             (for f-curr first 1 then f-next)
             (for f-prev first 1 then f-temp)
             (for f-temp = f-curr)
             (for f-next = (mod (+ f-curr f-prev) o))
             
             ;; k = 3 * 5^F_i mod P
             (let* ((exp-5 (power-mod 5 f-curr p))
                    (k (mod (* 3 exp-5) p))
                    ;; V = phi^k mod M
                    (v (power-mod phi k m))
                    (vi (power-mod v (- m 2) m))
                    ;; F_k = (phi^k - psi^k)/sqrt(5). For odd k, psi^k = -phi^-k.
                    ;; F_k = (V + Vi) / sqrt(5). L_k = V - Vi.
                    (fk (mod (* (+ v vi) invr) m))
                    (lk (mod (- v vi) m))
                    ;; p_n = F_k/2, q_n = L_k/2
                    (pn (mod (* fk inv2) m))
                    (qn (mod (* lk inv2) m))
                    ;; s(F_i) = p_n^5 + q_n^5 mod M
                    (s (mod (+ (power-mod pn 5 m) 
                               (power-mod qn 5 m)) 
                            m)))
               (setf total-sum (mod (+ total-sum s) m))))
    
    total-sum))

;; Execute the manifestation
;(format t "~A~%" (solve))

#+| Do it | (solve )
;→ 378401935


