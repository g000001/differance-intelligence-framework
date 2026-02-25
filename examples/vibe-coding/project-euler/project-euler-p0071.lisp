;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
;;; <p>Consider the fraction, $\dfrac n d$, where $n$ and $d$ are positive integers. If $n \lt d$ and $\operatorname{HCF}(n,d)=1$, it is called a reduced proper fraction.</p>
;;; <p>If we list the set of reduced proper fractions for $d \le 8$ in ascending order of size, we get:
;;; $$\frac 1 8, \frac 1 7, \frac 1 6, \frac 1 5, \frac 1 4, \frac 2 7, \frac 1 3, \frac 3 8, \mathbf{\frac 2 5}, \frac 3 7, \frac 1 2, \frac 4 7, \frac 3 5, \frac 5 8, \frac 2 3, \frac 5 7, \frac 3 4, \frac 4 5, \frac 5 6, \frac 6 7, \frac 7 8$$</p>
;;; <p>It can be seen that $\dfrac 2 5$ is the fraction immediately to the left of $\dfrac 3 7$.</p>
;;; <p>By listing the set of reduced proper fractions for $d \le 1\,000\,000$ in ascending order of size, find the numerator of the fraction immediately to the left of $\dfrac 3 7$.</p>


(cl:in-package cl-user)
(defpackage #:project-euler-0071 
  (:use cl #:iterate #|alexandria|#))
(in-package #:project-euler-0071)

(defun solve-ordered-fractions (&optional (limit 1000000))
  "Finds the numerator of the fraction immediately to the left of 3/7 
   for denominators up to LIMIT."
  (let ((target-n 3)
        (target-d 7))
    (numerator
     (iter (for d from 1 to limit)
           ;; 世俗諦（Conventional Truth）: 全てのdに対して最大のnを探索する。
           ;; 勝義諦（Ultimate Truth）: n/d < 3/7 は 7n < 3d、即ち 7n <= 3d - 1 と等価。
           (for n = (floor (1- (* d target-n)) target-d))
           ;; 0以下の分子は考慮しない（中道の現成）。
           (when (plusp n)
             ;; Exact Integer Projection: 
             ;; 浮動小数点を使わず、Lispの有理数型を用いることで、
             ;; 無限精度の比較を行い「幻影（誤差）」を排除する。
             (maximizing (/ n d)))))))

;; 実行
;; (solve-ordered-fractions 1000000)

#+| Do it | (solve-ordered-fractions 1000000)
#|------------------------------------------------------------|
Timing the evaluation of (solve-ordered-fractions 1000000)

User time    =        0.153
System time  =        0.007
Elapsed time =        0.128
Allocation   = 24184504 bytes
1073 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 428570
:ok

