;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview

(cl:in-package cl-user)
(defpackage #:project-euler-0820
  (:use #:cl #:iterate)
  (:export #:solve))
(in-package #:project-euler-0820)

#||
(cl-text project-euler-0820-analysis
  (cl-comment \"Problem 820: Sum of n-th decimal digits of 1/k\")

  ;; 1. 定義: 小数第n位の抽出 (Manifestation)
  (forall (n k d)
    (iff (is_nth_decimal_digit d n k)
         (equal d (floor (mod (div (pow 10 n) k) 10)))))

  ;; 2. 数学的還元: 効率的な計算式 (Middle Way / ACX Jump)
  ;; 浮動小数点を避け、整数論的剰余（10^{n-1} mod k）を用いる。
  (forall (n k)
    (equal (digit n k)
           (floor (div (mul 10 (mod (pow 10 (minus n 1)) k)) k))))

  ;; 3. 計算量の制約 (Avoid NMF)
  ;; n=10^7 のため、O(n^2) の文字列変換や全桁計算を禁止し、
  ;; O(n log n) のバイナリ法による剰余冪演算を採用する。
  (forall (a n)
    (if (and (Algorithm a) (Constraint n 10000000))
        (and (uses_modular_exponentiation a)
             (avoids_floating_point_errors a))))

  ;; 4. 創発 (Emergence)
  ;; S(n) は各独立した項の集積として定義される。
  (forall (n s)
    (iff (is_sum_S n s)
         (equal s (sum (map (lambda (k) (digit n k)) (range 1 n))))))
)
||#

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun fast-mod-expt (base power modulo)
  "二諦随伴プロトコル：世俗的な巨大数を避け、剰余の空間（空性）で冪乗を現成させる。"
  (declare (type (unsigned-byte 64) base power modulo))
  (if (= modulo 1)
      (return-from fast-mod-expt 0))
  (let ((res 1)
        (b (mod base modulo)))
    (declare (type (unsigned-byte 64) res b))
    (iterate
      (while (> power 0))
      (when (logbitp 0 power)
        (setf res (mod (* res b) modulo)))
      (setf b (mod (* b b) modulo))
      (setf power (ash power -1)))
    res))

(defun get-digit (n power k)
  "第n位の数字を計算する。powerは(1- n)を事前計算したもの。"
  (declare (type (unsigned-byte 64) n power k))
  (if (= k 1)
      0
      (let ((rem (fast-mod-expt 10 power k)))
        (declare (type (unsigned-byte 64) rem))
        ;; d_n(1/k) = floor(10 * (10^{n-1} mod k) / k)
        (values (floor (* 10 rem) k)))))

(defun solve (&optional (n 10000000))
  "S(n)を計算する。中道に基づき、メモリ消費を抑えた集積を行う。"
  (declare (type (unsigned-byte 64) n))
  (let ((power (1- n)))
    (iterate
      (for k from 1 to n)
      (summing (get-digit n power k)))))

;; 実行例:
;; (time (print (solve 7)))    ; => 10
;; (time (print (solve 100)))  ; => 418
;; (time (print (solve 10000000)))
#+| Do it | (solve )
;→ 44967734
:ok
