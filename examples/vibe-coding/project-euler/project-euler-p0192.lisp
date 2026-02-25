
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0192 (:use cl alexandria))
(in-package #:project-euler-0192)

(defun get-best-denominator (n bound)
  "分母の境界 bound 内で sqrt(n) のベスト近似の分母を返す"
  (let* ((a0 (isqrt n))
         ;; p_k / q_k の初期値 (C_{-1} = 1/0, C_0 = a0/1)
         (p-prev 1)
         (q-prev 0)
         (p-curr a0)
         (q-curr 1)
         ;; 連分数展開用の変数
         (m-cf 0)
         (d-cf 1)
         (a-cf a0))
    (loop
      ;; 次の連分数係数 a_{k+1} を計算
      (setf m-cf (- (* d-cf a-cf) m-cf))
      (setf d-cf (/ (- n (* m-cf m-cf)) d-cf))
      (setf a-cf (floor (+ a0 m-cf) d-cf))
      (let ((p-next (+ (* a-cf p-curr) p-prev))
            (q-next (+ (* a-cf q-curr) q-prev)))
        ;; 分母が境界を超えた場合、候補を比較する
        (when (> q-next bound)
          (let* (;; 境界を超えない最大の半収束分数の係数 m
                 (m-semi (floor (- bound q-prev) q-curr))
                 (p-semi (+ (* m-semi p-curr) p-prev))
                 (q-semi (+ (* m-semi q-curr) q-prev))
                 ;; 比較対象: A = p-curr/q-curr, B = p-semi/q-semi
                 ;; 中点 M = (A + B) / 2 = (p_curr*q_semi + p_semi*q_curr) / (2*q_curr*q_semi)
                 (mid-num (+ (* p-curr q-semi) (* p-semi q-curr)))
                 (q-prod (* q-curr q-semi))
                 ;; n と M^2 の比較: 4 * n * q-prod^2  vs  mid-num^2
                 (left (* 4 n q-prod q-prod))
                 (right (* mid-num mid-num)))
            (return-from get-best-denominator
              ;; A と B は sqrt(n) を挟んで反対側にある
              (if (< (* p-curr q-semi) (* p-semi q-curr))
                  ;; A < sqrt(n) < B の場合
                  (if (< left right) q-curr q-semi)
                  ;; B < sqrt(n) < A の場合
                  (if (> left right) q-curr q-semi)))))
        ;; 次のステップへ
        (setf p-prev p-curr
              q-prev q-curr
              p-curr p-next
              q-curr q-next)))))

(defun solve ()
  (let ((bound (expt 10 12))
        (total-sum 0))
    (loop for n from 2 to 100000
          for s = (isqrt n)
          ;; 平方数でない場合のみ計算
          unless (= (* s s) n)
          do (incf total-sum (get-best-denominator n bound)))
    total-sum))

;; (solve) を実行することで答えが得られます。

#+| Do it | (solve )
;→ 57060635927998347
