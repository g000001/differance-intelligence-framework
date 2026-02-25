;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0141 (:use cl alexandria))
(in-package #:project-euler-0141)

;; ==============================================================
;; SKDT – Dual Sunyata Structures and Progressive Numbers
;; --------------------------------------------------------------
;; Title   : Progressive Perfect Squares (Project Euler 141)
;; Author  : Masaomi Chiba
;; Date    : 2025-12-01
;;
;; [二諦随伴 (Two-Truths Entanglement) Protocol]
;; 世俗諦 (Conventional Truth): n = d*q + r, d,q,r は等比数列
;; 勝義諦 (Ultimate Truth): n = k^2*a*b^3 + k*a^2 (空性の還元)
;; 中道 (Middle Way): 探索空間の構築と完全平方数の現成
;; ==============================================================

(declaim (inline perfect-square-p))
(defun perfect-square-p (n)
  "nが完全平方数（現成した実体）であるかを確認する。"
  (declare (type (unsigned-byte 64) n)
           (optimize (speed 3) (safety 0)))
  (let ((root (isqrt n)))
    (= (* root root) n)))

(defun solve ()
  "10^12 未満の累進完全平方数の総和を求める。"
  (declare (optimize (speed 3) (safety 0)))
  (let ((limit (expt 10 12))
        (squares (make-hash-table :test 'eql)))
    (declare (type (unsigned-byte 64) limit))
    ;; 等比数列の項を a^2*k, a*b*k, b^2*k と置く (gcd(a,b)=1, b>a)
    ;; r < d の制約から、r = a^2*k または r = a*b*k
    ;; r = a*b*k の場合、n は完全平方数になり得ない。
    ;; したがって、n = (a*b*k)*(b^2*k) + a^2*k = a*b^3*k^2 + a^2*k のみを探索する。
    (loop for b from 2 below 10000
          for b3 of-type (unsigned-byte 64) = (* b b b)
          do (loop for a from 1 below b
                   when (= 1 (gcd a b))
                   do (let* ((ab3 (* a b3))
                             (a2 (* a a)))
                        (declare (type (unsigned-byte 64) ab3 a2))
                        (loop for k from 1
                              ;; n = k * (k * a * b^3 + a^2)
                              for n of-type (unsigned-byte 64) = (+ (* ab3 k k) (* a2 k))
                              while (< n limit)
                              do (when (perfect-square-p n)
                                   ;; 重複を避けるためハッシュテーブルに格納（現成の固定化）
                                   (setf (gethash n squares) t))))))
    ;; 総和の計算
    (let ((sum 0))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (incf sum k))
               squares)
      sum)))

(defun main ()
  "エントリポイント：計算結果を出力する。"
  (format t "~D~%" (solve)))

;; プログラムの実行
;; (main)

#+| Do it | (main )
;▻ 878454337159
;→ nil
