
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0955
  (:use #:cl #:iterate #:alexandria))
(in-package #:project-euler-0955)

;; ==============================================================================
;; 955. 三角数の連鎖 (SKDT Dual Sunyata Structures)
;; ==============================================================================
;; 問題の数列 a_n は以下の規則に従う：
;; 1. a_n が三角数 T_m = m(m+1)/2 のとき、a_{n+1} = a_n + 1
;; 2. そうでないとき、a_{n+1} = 2a_n - a_{n-1} + 1
;;
;; この規則から、a_n が三角数に達すると、差分数列がリセットされ、
;; 次の三角数 T_M に達するまで a_{n+k} = a_n + T_k となることが導かれる。
;; つまり、T_M = T_m + T_k を満たす最小の k > 1 を見つける問題に帰着する。
;;
;; 圏論的還元 (Aletheic Reduction):
;; 方程式 m(m+1) + k(k+1) = M(M+1) を変形すると、
;; (2k+1)^2 = (2M+1)^2 - 8T_m = (2M+1)^2 - 4m(m+1)
;; (2M+1)^2 - (2k+1)^2 = 4m(m+1)
;; (u+v)(v-u) = 4m(m+1) (ただし u = 2k+1, v = 2M+1)
;; ここで P = m(m+1) とし、uv = P となるペア {u, v} を探索する。
;; 条件：u+v が奇数（2の冪が一方に寄る）、かつ u+v > 2m+1。
;; ==============================================================================

(defun odd-part (n)
  "整数 n から 2 の冪因子を取り除いた奇数部分を返す（空性への還元）。"
  (if (zerop n) 0
      (let ((x n))
        (iterate (while (and (> x 0) (evenp x)))
                 (setf x (/ x 2)))
        x)))

(defun factorize (n)
  "奇数 n を素因数分解する（世俗の境界の特定）。"
  (if (<= n 1) nil
      (let ((factors '())
            (temp n))
        (iterate (for d from 3 by 2)
                 (while (<= (* d d) temp))
                 (when (zerop (rem temp d))
                   (let ((count 0))
                     (iterate (while (zerop (rem temp d)))
                              (incf count)
                              (setf temp (/ temp d)))
                     (push (cons d count) factors))))
        (if (> temp 1) (push (cons temp 1) factors))
        factors)))

(defun get-divisors (factors)
  "素因数分解リストからすべての約数を生成する（創発的空間の構築）。"
  (if (null factors)
      (list 1)
      (let ((remaining (get-divisors (cdr factors)))
            (p (caar factors))
            (e (cdar factors)))
        (iterate (for i from 0 to e)
                 (for p-pow initially 1 then (* p-pow p))
                 (appending
                  (iterate (for r in remaining)
                           (collect (* r p-pow))))))))

(defun solve-project-euler-0955 (&optional (target-count 70))
  "第 target-count 番目の三角数のインデックス n を求める（中道の現成）。"
  (let ((m 2)  ; a_0 = 3 = T_2
        (n 0)) ; a_0 のインデックス
    (iterate (for i from 2 to target-count)
             (let* ((p (* m (1+ m)))
                    ;; m と m+1 は互いに素なので、個別に分解して結合できる
                    (f-m (factorize (odd-part m)))
                    (f-m1 (factorize (odd-part (1+ m))))
                    (divs (get-divisors (append f-m f-m1)))
                    (s-crit (1+ (* 2 m)))
                    (best-s nil)
                    (best-k nil))
               ;; S = u + v を最小化する（ただし S > 2m+1 かつ S は奇数）
               ;; u+v が奇数になるのは、P = 2^s * O において 2^s が u か v のどちらかに
               ;; 完全に含まれる場合のみ。これは {d, P/d} (dはOの約数) の形に等しい。
               (iterate (for d in divs)
                        (let* ((u d)
                               (v (/ p d))
                               (s (+ u v)))
                          (when (> s s-crit)
                            (when (or (null best-s) (< s best-s))
                              (setf best-s s
                                    best-k (/ (1- (abs (- v u))) 2))))))
               ;; 状態の更新（随伴による遷移）
               (setf m (/ (1- best-s) 2)
                     n (+ n best-k))))
    n))

;; 実行
;; (format t "Index of the 70th triangle number: ~A~%" (solve-project-euler-0955 70))

(eval-when (:execute)
  (format t "~A~%" (solve-project-euler-0955 70)))
;▻ 6795261671274
;→ nil

:ok