;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0282 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0282)

#||
(cl-text "Project Euler 282 Logic Projection"
  (cl-comment "1. Exact Substructure Construction: Ackermann function values are mapped to power towers. A(n,n) corresponds to Knuth's up-arrow notation, reducing evaluation to finite modular arithmetic.")
  (forall (n m)
    (if (>= n 4)
        (= (A n n) (- (power_tower (height n) 2) 3))))

  (cl-comment "2. Euler's Totient Theorem Extension: For a^b mod m where b >= log2(m), we use b' = (b mod phi(m)) + phi(m) to correctly handle non-coprime bases. Towers height >= 5 trivially satisfy b >= 65536 > log2(14^8).")
  (forall (h m)
    (if (>= h 5)
        (= (tower_mod h m)
           (power_mod 2 (+ (tower_mod (- h 1) (phi m)) (phi m)) m))))

  (cl-comment "3. Asymptotic Convergence: Power towers converge modulo m after at most 24 iterations of phi(m). A(5,5) and A(6,6) reach this infinite tower state.")
  (= (mod (A 5 5) m) (mod (- (tower_mod 100 m) 3) m))
)
||#


;;; =========================================================
;;; Euler's Totient Function: phi(n)
;;; =========================================================
(defun phi (n)
  (let ((result n)
        (m n))
    (iterate
      (for i from 2)
      (while (<= (* i i) m))
      (when (= (mod m i) 0)
        (setf result (- result (truncate result i)))
        (iterate (while (= (mod m i) 0)) (setf m (truncate m i)))))
    (if (> m 1)
        (- result (truncate result m))
        result)))

;;; =========================================================
;;; Modular Exponentiation: base^exp mod m
;;; =========================================================
(defun power-mod (base exp m)
  (let ((res 1)
        (b (mod base m))
        (e exp))
    (iterate
      (while (> e 0))
      (when (oddp e)
        (setf res (mod (* res b) m)))
      (setf b (mod (* b b) m)
            e (ash e -1)))
    res))

;;; =========================================================
;;; Power Tower Evaluation Modulo m: 2^^h mod m
;;; =========================================================
(defun tower-mod (h m)
  (cond ((= m 1) 0)
        ((= h 1) (mod 2 m))
        ((= h 2) (mod 4 m))
        ((= h 3) (mod 16 m))
        ;; h=4 のとき、値は 65536 となる。
        ((= h 4) (mod 65536 m))
        ;; h>=5 のとき、前のタワーの高さは最低でも 65536 となる。
        ;; モジュロ m <= 14^8 において、指数 b = 65536 は常に b >= log2(m) の条件を満たす。
        ;; したがってオイラーの定理の拡張 a^b = a^{b mod phi(m) + phi(m)} mod m が常に成立する。
        (t (let* ((ph (phi m))
                  (exp (tower-mod (1- h) ph)))
             (power-mod 2 (+ exp ph) m)))))

;;; =========================================================
;;; Core Solver
;;; =========================================================
(defun solve ()
  (let ((m (expt 14 8))) ;; M = 14^8 = 1475789056
    
    (format t "Modulo M = ~A~%" m)
    
    ;; A(n, n) の値の還元
    ;; A(0,0) = 1
    ;; A(1,1) = 3
    ;; A(2,2) = 7
    ;; A(3,3) = 61
    ;; A(4,4) = 2^^7 - 3
    ;; A(5,5) = 2^^(A(5,4)+3) - 3 => 実質的に無限のタワー
    ;; A(6,6) = 2^^(A(6,5)+3) - 3 => 実質的に無限のタワー
    
    (let ((a0 1)
          (a1 3)
          (a2 7)
          (a3 61)
          ;; (mod (- X 3) m) により負数を適切に正のモジュロ空間へ射影する
          (a4 (mod (- (tower-mod 7 m) 3) m))
          ;; 24回の再帰で phi(m) は 1 に収束するため、高さ100で「無限タワー」と同義になる
          (a5 (mod (- (tower-mod 100 m) 3) m)) 
          (a6 (mod (- (tower-mod 100 m) 3) m)))
      
      (format t "A(0,0) = ~A~%" a0)
      (format t "A(1,1) = ~A~%" a1)
      (format t "A(2,2) = ~A~%" a2)
      (format t "A(3,3) = ~A~%" a3)
      (format t "A(4,4) mod M = ~A~%" a4)
      (format t "A(5,5) mod M = ~A~%" a5)
      (format t "A(6,6) mod M = ~A~%" a6)
      
      (mod (+ a0 a1 a2 a3 a4 a5 a6) m))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Modulo M = 1475789056
A(0,0) = 1
A(1,1) = 3
A(2,2) = 7
A(3,3) = 61
A(4,4) mod M = 915627005
A(5,5) mod M = 829575165
A(6,6) mod M = 829575165

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 1168 bytes
37 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1098988351
:ok