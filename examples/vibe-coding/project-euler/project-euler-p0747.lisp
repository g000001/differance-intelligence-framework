;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0747 (:use cl alexandria) (:export #:solve))
(in-package #:project-euler-0747)

#||
【自己批判と数論的ショートカットの証明】
ピザを三角形に分割するという幾何学的制約は、コーナーの処理に基づく二次方程式の根の存在条件に写像される。
さらに、Σψ(n) という巨大な和を n について先に評価することで、
4ab <= N を満たす (a,b) のペアに対する O(N log N) の純粋な算術ループに完全崩壊する。
実行中の最大値である 4a(a-1)b(b-1) は 2.5 * 10^15 程度であり、
Lispの 62bit fixnum に安全に収まるため、Bignum アロケーションを完全に排除した最速の実行が可能となる。
||#

(defconstant +mod+ 1000000007)

(defun power-mod (base exp)
  (declare (type fixnum base exp))
  (let ((res 1)
        (b (mod base +mod+)))
    (declare (type fixnum res b))
    (loop while (> exp 0) do
      (when (oddp exp) (setf res (mod (* res b) +mod+)))
      (setf b (mod (* b b) +mod+))
      (setf exp (ash exp -1)))
    res))

(defun solve (&optional (N #.(expt 10 8)))
  (let ((ans 0)
        (T-sum 0))
    (declare (type fixnum ans T-sum))
    
    ;; 1. 多項式部分の和の O(1) 計算
    (let* ((N-mod (mod N +mod+))
           (N+1-mod (mod (+ N 1) +mod+))
           (2N+1-mod (mod (+ (* 2 N) 1) +mod+))
           (sum-sq (mod (* N-mod (mod (* N+1-mod 2N+1-mod) +mod+)) +mod+))
           (inv6 (power-mod 6 (- +mod+ 2)))
           (term1 (mod (* sum-sq inv6) +mod+))
           
           (sum-n (mod (* N-mod N+1-mod) +mod+))
           (inv2 (power-mod 2 (- +mod+ 2)))
           (term2 (mod (* 9 (mod (* sum-n inv2) +mod+)) +mod+))
           
           (term3 (mod (* 22 N-mod) +mod+))
           
           (poly-sum (mod (+ term1 term2 (- +mod+ term3) 12) +mod+)))
      (setf ans (mod (* poly-sum inv2) +mod+)))
      
    ;; 2. 非自明な根の数 R(n,a,b) の和 (O(N log N) ループ)
    (do ((a 2 (1+ a)))
        (nil)
      (declare (type fixnum a))
      (let ((Pa (* a (- a 1))))
        ;; a の上限チェック
        (let* ((F (- (* 4 a a) (* 4 a)))
               (C (+ (* 2 (- N 1 F)) 1)))
          (declare (type fixnum F C))
          (when (<= C 0)
            (return)))
            
        (let ((inner-sum 0))
          (declare (type fixnum inner-sum))
          (do ((b a (1+ b)))
              (nil)
            (declare (type fixnum b))
            (let* ((Pb (* b (- b 1)))
                   (P (* Pa Pb))
                   (P4 (ash P 2))
                   (S4 (isqrt P4))
                   (I (if (= (* S4 S4) P4) 1 0))
                   (F (+ (- (* 2 a b) a b) S4))
                   (C (+ (* 2 (- N 1 F)) I)))
              (declare (type fixnum F C I))
              
              ;; N-1 を超えたら b の探索を打ち切る
              (when (<= C 0)
                (return))
                
              (let ((add-val (if (= a b) C (* 2 C))))
                (declare (type fixnum add-val))
                (incf inner-sum add-val))))
                
          ;; 巨大な inner-sum も modulo を取って足し合わせる
          (setf T-sum (mod (+ T-sum inner-sum) +mod+)))))
          
    (setf ans (mod (+ ans (* 3 T-sum)) +mod+))
    (format t "Psi(~D) = ~D~%" N ans)
    ans))

#+| Do it | (project-euler-0747:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Psi(100000000) = 681813395

User time    =       55.011
System time  =        0.628
Elapsed time =       55.311
Allocation   = 912312 bytes
13325 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 681813395
:ok