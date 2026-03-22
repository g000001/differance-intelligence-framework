;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0620 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0620)

#||
Project Euler 620: Planetary Gears
真の数論的ショートカットと次元崩壊:
各 $s, p, q$ の組み合わせに対し、$A = s+p$、$B = s+q$ とする。
SとCが最も離れる最大距離のスケール値は $x_{max} = p+q-2\pi$ と、sに依存しない定数になる。
この $x_{max}$ における不変量 $K_p = \frac{A \theta - B \phi}{\pi}$ を求める。
配置可能な条件は、K_pが整数kを取ることである。
K_pは x の増加に対し単調減少であり、最小有効距離 x = B-A の極限で K_p = A となるため、
可能な配置の数（有効な k の個数）は、半開区間 [ K_p(x_{max}), A ) に含まれる整数の数、
すなわち (A - 1) - floor(K_p(x_{max})) と極めてシンプルに求まる。
||#

(defun solve ()
  (let ((limit 500)
        (total-arrangements 0))
    
    (format t "Starting evaluation for limit = ~D...~%" limit)
    
    ;; ループ最適化: x_max は s に依存しないため p, q を外側に配置
    (iterate (for p from 5 to limit)
      (iterate (for q from (1+ p) to limit)
        (when (> (+ 5 p q) limit)
          (leave))
        
        (let* ((x (- (+ p q) (* 2 pi)))
               (x-sq (* x x)))
          
          (iterate (for s from 5 to limit)
            (when (> (+ s p q) limit)
              (leave))
            
            (let* ((A (+ s p))
                   (B (+ s q))
                   (A-sq (* A A))
                   (B-sq (* B B))
                   
                   ;; 余弦定理（正しい符号）
                   (cos-theta (/ (- (+ A-sq x-sq) B-sq) (* 2 A x)))
                   (cos-phi (/ (- (+ B-sq x-sq) A-sq) (* 2 B x)))
                   
                   ;; IEEE 754 の丸め誤差によるドメインエラー防止
                   (theta (acos (max -1 (min 1 cos-theta))))
                   (phi (acos (max -1 (min 1 cos-phi))))
                   
                   ;; 位相不変量 K_p の計算
                   (K-p (/ (- (* A theta) (* B phi)) pi))
                   
                   ;; 有効な配置の数は、区間内の整数の個数
                   (valid-k-count (- A 1 (floor K-p))))
              
              (when (> valid-k-count 0)
                (incf total-arrangements valid-k-count)))))))
                  
    (format t "Calculation complete. ~%")
    total-arrangements))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting evaluation for limit = 500...
Calculation complete. 

User time    =        3.947
System time  =        0.049
Elapsed time =        3.915
Allocation   = 7368434096 bytes
376 Page faults
GC time      =        0.097
 |------------------------------------------------------------|#
;;→ 1470337306
:ok