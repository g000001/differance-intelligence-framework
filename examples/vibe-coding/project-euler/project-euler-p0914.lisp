
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0914 (:use cl alexandria))
(in-package #:project-euler-0914)

(defun solve-f (r)
  "Find the largest inradius F(R) for a given radius R."
  (let* ((c-limit (1- (* 2 r)))
         ;; 連続空間での最適解 m/n = 1 + sqrt(2) に基づく開始点の推定
         ;; m = sqrt(c) * cos(pi/8)
         (m-opt (round (* (sqrt (float c-limit 1.0d0))
                          (sqrt (/ (+ 2.0d0 (sqrt 2.0d0)) 4.0d0)))))
         (max-r 0)
         ;; 離散的な原始条件による変動をカバーするため、十分な探索範囲を設定
         (search-range 200000))
    (loop for m from (- m-opt search-range) to (+ m-opt search-range)
          do (let* ((m2 (* m m))
                    (rem (- c-limit m2)))
               (when (plusp rem)
                 (let ((n-limit (isqrt rem)))
                   ;; n(m-n) は n < m/2 の範囲で単調増加するため、
                   ;; 制約を満たす最大の n から逆順に探索を開始する
                   (loop for n from n-limit downto 1
                         when (and (oddp (+ m n))
                                   (= 1 (gcd m n)))
                         do (let ((current-r (* n (- m n))))
                              (when (> current-r max-r)
                                (setf max-r current-r)))
                            ;; 条件を満たす最大の n がその m における最適解
                            (return))))))
    max-r))

(defun main ()
  (let ((result (solve-f (expt 10 18))))
    (format t "F(10^18) = ~A~%" result)))

;; (main) を実行することで答えが得られます。

#+| Do it | (main )
;▻ F(10^18) = 414213562371805310
;→ nil

