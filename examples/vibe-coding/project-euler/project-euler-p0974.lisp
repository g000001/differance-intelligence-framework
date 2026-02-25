
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0974 (:use cl alexandria))
(in-package #:project-euler-0974)

;; ==============================================================
;; SKDT – Dual Sunyata Structures and Emergent Category Theory
;; --------------------------------------------------------------
;; Title   : Emergence of the 10^16th Very Odd Number
;; Author  : Masaomi Chiba
;; Date    : 2025-12-01
;; ==============================================================

;; "Very odd number" の定義:
;; 1. 奇数の桁のみを含む (1, 3, 5, 7, 9)
;; 2. 105 で割り切れる (105 = 3 * 5 * 7)
;; 3. 各奇数の桁が奇数回出現する
;;
;; 解析:
;; - 奇数の桁のみで 105 の倍数であるため、末尾の桁は 5 で確定する。
;; - 105 の倍数であることは、5 の倍数（末尾が5で充足）かつ 21 の倍数であることを意味する。
;; - 全ての奇数桁 (1, 3, 5, 7, 9) が奇数回出現するため、全体の桁数 L は奇数である。
;; - 末尾が 5 であるため、残りの L-1 個の桁（プレフィックス）において:
;;   - 1, 3, 7, 9 の出現回数は奇数。
;;   - 5 の出現回数は偶数。
;; - プレフィックスの値を V とすると、(V * 10 + 5) ≡ 0 (mod 21) より
;;   10V ≡ -5 ≡ 16 (mod 21) => V ≡ 10 (mod 21) となる。

(defun power-10-mod-21 (k)
  "10^k mod 21 を計算する。"
  (mod (expt 10 k) 21))

(defun solve-theta (target-n)
  "n番目の Very odd number を求める。"
  (let* ((max-len 60) ; 10^16 に対して十分な長さを確保
         ;; dp[len][rem][parity]
         ;; len: プレフィックスの長さ
         ;; rem: 21 で割った余り (0..20)
         ;; parity: 各桁の出現回数のパリティ (5ビット mask)
         ;;   bit 0: '1', bit 1: '3', bit 2: '5', bit 3: '7', bit 4: '9'
         (dp (make-array (list (1+ max-len) 21 32) :initial-element 0))
         (odd-digits '(1 3 5 7 9))
         (target-parity 27)) ; binary 11011 (1,3,7,9 が奇数、5 が偶数)

    ;; 1. DP テーブルの構築 (空性からの現成)
    (setf (aref dp 0 0 0) 1)
    (dotimes (l max-len)
      (dotimes (r 21)
        (dotimes (p 32)
          (let ((count (aref dp l r p)))
            (when (> count 0)
              (dolist (d odd-digits)
                (let ((nr (mod (+ (* r 10) d) 21))
                      (np (logxor p (ash 1 (floor d 2)))))
                  (incf (aref dp (1+ l) nr np) count))))))))

    ;; 2. ターゲットの桁数 L を特定する (中道の探索)
    (let ((total 0)
          (prefix-len 0)
          (n target-n))
      ;; L は奇数なので prefix-len (L-1) は偶数
      (loop for l from 6 by 2 to max-len
            for c = (aref dp l 10 target-parity)
            do (if (>= (+ total c) n)
                   (progn
                     (setf prefix-len l)
                     (setf n (- n total))
                     (return))
                   (incf total c)))

      ;; 3. 桁を決定する (世俗諦への固定化)
      (let ((curr-v 0)
            (curr-p 0)
            (res-prefix nil))
        (loop for j from prefix-len downto 1
              do (dolist (d odd-digits)
                   (let* ((v (mod (+ (* curr-v 10) d) 21))
                          (p (logxor curr-p (ash 1 (floor d 2))))
                          (rem-len (1- j))
                          ;; 残りの桁で必要な余りとパリティを逆算
                          (target-v (mod (- 10 (mod (* v (power-10-mod-21 rem-len)) 21)) 21))
                          (target-p (logxor target-parity p))
                          (c (aref dp rem-len target-v target-p)))
                     (if (>= c n)
                         (progn
                           (push d res-prefix)
                           (setf curr-v v)
                           (setf curr-p p)
                           (return))
                         (setf n (- n c))))))
        
        ;; プレフィックスに末尾の 5 を結合して返す
        (format nil "~{~D~}5" (reverse res-prefix))))))

;; 実行と出力
;(format t "Theta(1)    = ~A~%" (solve-theta 1))
;(format t "Theta(10^3) = ~A~%" (solve-theta 1000))
;(format t "Theta(10^16) = ~A~%" (solve-theta 10000000000000000))

#+| Do it | (solve-theta 10000000000000000)
;→ "13313751171933973557517973175"


