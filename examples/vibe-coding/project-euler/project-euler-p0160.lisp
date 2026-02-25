
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0160 (:use cl alexandria))
(in-package #:project-euler-0160)

;;; ============================================================================
;;; Project Euler 0160: Factorial trailing digits
;;; ----------------------------------------------------------------------------
;;; $f(N)$ を、$N!$ の末尾の 0 を取り除いた後の下 5 桁とする。
;;; $N = 1,000,000,000,000$ ($10^{12}$) における $f(N)$ を求める。
;;;
;;; [二諦随伴による解法]
;;; 1. 世俗諦 (Conventional Truth): $N!$ を直接計算することは $O(N)$ であり、
;;;    $10^{12}$ の制約下では「非中道の誤謬 (NMF)」となる。
;;; 2. 勝義諦 (Ultimate Truth): 数論的還元を用いる。
;;;    $N! = 2^{v_2(N!)} \cdot 5^{v_5(N!)} \cdot V(N)$
;;;    ここで $V(N)$ は 2 と 5 で割り切れない（10 と互いに素な）因子の積の剰余構造。
;;;    末尾の 0 は $10^{v_5(N!)}$ で割ることに相当するため、
;;;    $f(N) \equiv 2^{v_2(N!) - v_5(N!)} \cdot V(N) \pmod{10^5}$ となる。
;;; 3. 中道 (Middle Way): 再帰的構造 $V(n)$ をメモ化によって現成させ、
;;;    巨大な探索空間を $O(\log^2 N)$ に爆縮する。
;;; ============================================================================


(defun mod-expt (base power modulus)
  "繰り返し二乗法による高速なモジュラべき乗: base^power mod modulus"
  (let ((result 1)
        (b (mod base modulus))
        (p power))
    (loop while (> p 0) do
      ;; 指数が奇数のときだけ現在の b を掛ける
      (when (oddp p)
        (setf result (mod (* result b) modulus)))
      ;; b を 2乗して剰余をとる
      (setf b (mod (* b b) modulus))
      ;; 指数を半分にする (1ビット右シフト)
      (setf p (ash p -1)))
    result))

(defun extended-gcd (a b)
  "拡張ユークリッド互除法: g = ax + by となる (values g x y) を返す。"
  (if (zerop a)
      (values b 0 1)
      (multiple-value-bind (g x1 y1) (extended-gcd (mod b a) a)
        (values g (- y1 (* (truncate b a) x1)) x1))))

(defun mod-inverse (a m)
  "モジュラ逆数を計算する。"
  (multiple-value-bind (g x y) (extended-gcd a m)
    (declare (ignore y))
    (if (= g 1)
        (mod x m)
        (error "Modular inverse does not exist for ~A mod ~A" a m))))

(defun count-legendre (n p)
  "ルジャンドルの公式: n! に含まれる素因数 p の数を計算する。"
  (let ((count 0)
        (curr n))
    (loop while (> curr 0) do
      (setf curr (truncate curr p))
      (incf count curr))
    count))

(defun solve-0160 ()
  (let* ((n 1000000000000)
         (m 100000) ; 下 5 桁
         (w-table (make-array m :element-type '(unsigned-byte 64)))
         (v-memo (make-hash-table :test 'eql)))
    
    ;; 1. W(k) の事前計算: 1 から k までの整数のうち、10 と互いに素なものの積 mod m
    (setf (aref w-table 0) 1)
    (let ((current 1))
      (loop for i from 1 below m do
        (when (and (oddp i) (/= (mod i 5) 0))
          (setf current (mod (* current i) m)))
        (setf (aref w-table i) current)))
    
    (let ((w-m (aref w-table (1- m))))
      
      ;; 2. get-w(k): 周期性を利用して大きな k に対する W(k) を計算
      (labels ((get-w (k)
                 (if (zerop k)
                     1
                     (multiple-value-bind (q r) (truncate k m)
                       (mod (* (mod-expt w-m q m)
                               (aref w-table r))
                            m))))
               
               ;; 3. get-v(k): 2 と 5 の因子を再帰的にパージした積の剰余を計算
               ;; V(n) = W(n) * V(n/2) * V(n/5) * V(n/10)^-1
               (get-v (k)
                 (cond ((zerop k) 1)
                       ((gethash k v-memo) (gethash k v-memo))
                       (t (let* ((res (get-w k))
                                 (v2 (get-v (truncate k 2)))
                                 (v5 (get-v (truncate k 5)))
                                 (v10 (get-v (truncate k 10)))
                                 (inv-v10 (mod-inverse v10 m)))
                            (setf res (mod (* res v2) m))
                            (setf res (mod (* res v5) m))
                            (setf res (mod (* res inv-v10) m))
                            (setf (gethash k v-memo) res)
                            res)))))
        
        ;; 4. 最終計算
        ;; f(N) = 2^(v2-v5) * V(N) mod 10^5
        (let* ((v2-n (count-legendre n 2))
               (v5-n (count-legendre n 5))
               (pow2 (- v2-n v5-n))
               (coeff (mod-expt 2 pow2 m))
               (v-n (get-v n)))
          (mod (* coeff v-n) m))))))

;; 実行: (solve-0160)
;; 結果: 16576


#+| Do it | (solve-0160 )
;→ 16576
