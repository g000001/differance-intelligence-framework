;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0812 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0812)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【不変量と数論的ショートカットの証明】
力学多項式の根は1の冪根から作られる代数的整数に限定される。
写像 x -> x^2 - 2 の下での根の軌道を追跡することで、多項式の因数の多重度 c_m は
c_{2m} <= c_m (ただし m=2 のときのみ c_4 <= 2c_2) という局所的な非増加条件に完全に還元される。
これを生成関数にマッピングし、オイラー変換(対数微分)を用いることで、
O(N!)の組み合わせ爆発をO(N^2)の純粋な畳み込みDPへと完全に崩壊させる。
||#

(defconstant $modulo 998244353)

(defun make-uint64-array (size)
  (make-array size :element-type '(unsigned-byte 64) :initial-element 0))

(defun power-mod (base exp)
  "base^exp mod $modulo を計算する"
  (let ((res 1)
        (b (mod base $modulo)))
    (do ((e exp (ash e -1)))
        ((<= e 0) res)
      (when (oddp e)
        (setf res (mod (* res b) $modulo)))
      (setf b (mod (* b b) $modulo)))))

(defun solve (&optional (limit-n 10000))
  (let* ((safe-n (+ limit-n 5))
         ;; N=10000のとき、v=phi(d)/2 <= 10000 を満たす最大の奇数dは 60000未満
         (max-d 100005) 
         (phi (make-uint64-array max-d))
         (c-arr (make-uint64-array safe-n))
         (m-arr (make-uint64-array safe-n))
         (a-arr (make-uint64-array safe-n))
         (f-arr (make-uint64-array safe-n))
         (g1-arr (make-uint64-array safe-n)))

    ;; 1. G1(q) の計算 (d=1 の寄与を生成関数から展開)
    (setf (aref g1-arr 0) 1)
    (let ((power 1))
      (do () ((> (ash 1 power) limit-n))
        (let ((step (ash 1 power)))
          (iterate ((index-i (scan-range :from step :upto limit-n)))
            (setf (aref g1-arr index-i)
                  (mod (+ (aref g1-arr index-i) (aref g1-arr (- index-i step))) $modulo))))
        (incf power)))

    ;; 1/(1-q)^2 を掛ける (2回の累積和)
    (iterate ((index-i (scan-range :from 1 :upto limit-n)))
      (setf (aref g1-arr index-i)
            (mod (+ (aref g1-arr index-i) (aref g1-arr (1- index-i))) $modulo)))
    (iterate ((index-i (scan-range :from 1 :upto limit-n)))
      (setf (aref g1-arr index-i)
            (mod (+ (aref g1-arr index-i) (aref g1-arr (1- index-i))) $modulo)))

    ;; 全体を 1/2 倍し、定数項に 1/2 を足す
    (let ((inv2 (truncate (1+ $modulo) 2)))
      (iterate ((index-i (scan-range :from 0 :upto limit-n)))
        (setf (aref g1-arr index-i)
              (mod (* (aref g1-arr index-i) inv2) $modulo)))
      (setf (aref g1-arr 0)
            (mod (+ (aref g1-arr 0) inv2) $modulo)))

    ;; 2. Sieve(篩) でオイラーの phi 関数を O(M log log M) で事前計算
    (iterate ((index-i (scan-range :from 0 :upto (1- max-d))))
      (setf (aref phi index-i) index-i))
    (iterate ((index-i (scan-range :from 2 :upto (1- max-d))))
      (when (= (aref phi index-i) index-i)
        (iterate ((index-j (scan-range :from index-i :upto (1- max-d) :by index-i)))
          (setf (aref phi index-j)
                (- (aref phi index-j) (truncate (aref phi index-j) index-i))))))

    ;; 3. C_v の計算 (phi(d)/2 = v となる奇数 d >= 3 の個数)
    (iterate ((index-d (scan-range :from 3 :upto (1- max-d) :by 2)))
      (let ((val-v (truncate (aref phi index-d) 2)))
        (when (<= val-v limit-n)
          (incf (aref c-arr val-v)))))

    ;; 4. M_w の計算 (アイテムサイズ w の総多重度)
    (iterate ((index-w (scan-range :from 1 :upto limit-n)))
      (let ((val-v index-w)
            (sum 0))
        (do () (nil)
          (setf sum (mod (+ sum (aref c-arr val-v)) $modulo))
          (when (oddp val-v)
            (return))
          (setf val-v (truncate val-v 2)))
        (setf (aref m-arr index-w) sum)))

    ;; 5. A_n の計算 (オイラー変換による対数微分の係数 A_n = sum_{w|n} w * M_w)
    (iterate ((index-i (scan-range :from 1 :upto limit-n)))
      (iterate ((index-j (scan-range :from index-i :upto limit-n :by index-i)))
        (setf (aref a-arr index-j)
              (mod (+ (aref a-arr index-j) (* index-i (aref m-arr index-i))) $modulo))))

    ;; 6. f_n の計算 (n * f_n = sum_{k=1}^n A_k * f_{n-k} による O(N^2) DP)
    (setf (aref f-arr 0) 1)
    (iterate ((index-n (scan-range :from 1 :upto limit-n)))
      (let ((sum 0))
        (iterate ((index-k (scan-range :from 1 :upto index-n)))
          (setf sum (+ sum (* (aref a-arr index-k) (aref f-arr (- index-n index-k)))))
          ;; Bignum回避のマイクロ最適化: 15回に1回だけ mod をとり、Lisp本来の高速な算術を活かす
          (when (= (logand index-k 15) 0)
            (setf sum (mod sum $modulo))))
        (setf sum (mod sum $modulo))
        (setf (aref f-arr index-n)
              (mod (* sum (power-mod index-n (- $modulo 2))) $modulo))
        (when (= (mod index-n 2000) 0)
          (format t "観測: f_n の計算 ~D / ~D 完了~%" index-n limit-n))))

    ;; 7. 最終的な畳み込み: S(N) = sum_{i=0}^N G1[i] * f[N-i]
    (let ((ans 0))
      (iterate ((index-i (scan-range :from 0 :upto limit-n)))
        (setf ans (mod (+ ans (* (aref g1-arr index-i) (aref f-arr (- limit-n index-i)))) $modulo)))
      (format t "S(~D) = ~D~%" limit-n ans)
      ans)))

#+| Do it | (project-euler-0812:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: f_n の計算 2000 / 10000 完了
観測: f_n の計算 4000 / 10000 完了
観測: f_n の計算 6000 / 10000 完了
観測: f_n の計算 8000 / 10000 完了
観測: f_n の計算 10000 / 10000 完了
S(10000) = 986262698

User time    =        1.482
System time  =        0.018
Elapsed time =        1.445
Allocation   = 1362752 bytes
304 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 986262698
:ok
