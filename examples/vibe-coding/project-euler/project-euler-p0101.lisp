;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0101 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0101)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defun u-poly (n)
  "問題で与えられた10次多項式 u_n = 1 - n + n^2 - ... + n^10 を計算する。
   等比数列の和の公式 (1 - (-n)^11) / (1 + n) を用いることも可能だが、
   nが小さいため直接計算の精度と確実性を優先する。"
  (iterate (for i from 0 to 10)
           (sum (expt (- n) i))))

(defun n-choose-k (n k)
  "二項係数 nCk を整数演算で計算する。"
  (if (or (< k 0) (> k n))
      0
      (let ((res 1)
            (k (if (> k (truncate n 2)) (- n k) k)))
        (iterate (for i from 1 to k)
                 (setf res (truncate (* res (+ n 1 (- i))) i)))
        res)))

(defun solve ()
  "ラグランジュ補間の特殊解を用いて FIT (First Incorrect Term) の総和を求める。
   計算量は O(D^2) であり、D=10 のため極めて高速に終了する。"
  (let* ((max-degree 10)
         (u-values (make-array (+ max-degree 2) :initial-element 0))
         (total-sum-of-fits 0))
    
    ;; 1. 多項式の値を事前計算 (n=1 から n=11 まで)
    (iterate (for n from 1 to (1+ max-degree))
             (setf (aref u-values n) (u-poly n)))
    
    ;; 2. 各 k (1 <= k <= 10) に対して OP(k, k+1) を計算
    (iterate (for k from 1 to max-degree)
             ;; 数論的ショートカット: OP(k, k+1) = Σ_{j=1..k} u_j * binom(k, j-1) * (-1)^(k-j)
             (for fit = (iterate (for j from 1 to k)
                                 (for coeff = (n-choose-k k (1- j)))
                                 (for term = (* (aref u-values j) coeff))
                                 (if (evenp (- k j))
                                     (sum term)
                                     (sum (- term)))))
             
             ;; 3. デバッグ出力: 推測値と実際の値の比較
             (let ((actual (aref u-values (1+ k))))
               (format t "[Debug] k=~2d: OP(k, k+1)=~15d, u_{k+1}=~15d" k fit actual)
               (if (/= fit actual)
                   (progn
                     (format t " -> BOP detected.~%")
                     (incf total-sum-of-fits fit))
                   (format t " -> Correct prediction (No BOP).~%"))))
    
    (format t "--------------------------------------------------~%")
    (format t "Final Sum of FITs: ~d~%" total-sum-of-fits)
    total-sum-of-fits))

;; 実行例: (project-euler-0101:solve)


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[Debug] k= 1: OP(k, k+1)=              1, u_{k+1}=            683 -> BOP detected.
[Debug] k= 2: OP(k, k+1)=           1365, u_{k+1}=          44287 -> BOP detected.
[Debug] k= 3: OP(k, k+1)=         130813, u_{k+1}=         838861 -> BOP detected.
[Debug] k= 4: OP(k, k+1)=        3092453, u_{k+1}=        8138021 -> BOP detected.
[Debug] k= 5: OP(k, k+1)=       32740951, u_{k+1}=       51828151 -> BOP detected.
[Debug] k= 6: OP(k, k+1)=      205015603, u_{k+1}=      247165843 -> BOP detected.
[Debug] k= 7: OP(k, k+1)=      898165577, u_{k+1}=      954437177 -> BOP detected.
[Debug] k= 8: OP(k, k+1)=     3093310441, u_{k+1}=     3138105961 -> BOP detected.
[Debug] k= 9: OP(k, k+1)=     9071313571, u_{k+1}=     9090909091 -> BOP detected.
[Debug] k=10: OP(k, k+1)=    23772343751, u_{k+1}=    23775972551 -> BOP detected.
--------------------------------------------------
Final Sum of FITs: 37076114526

User time    =        0.000
System time  =        0.000
Elapsed time =        0.001
Allocation   = 8984 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 37076114526
:ok