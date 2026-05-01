;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-1.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0116 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0116)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)

#||
(cl-comment "=== 6. Exact Integer Projection (勝義的整数化による浮動小数点の排除) ===")
(cl-comment "本問題は組合せ論的な数え上げであり、浮動小数点演算を一切介在させず、")
(cl-comment "整数の加算のみによる動的計画法（DP）へ還元する。")

(cl-comment "=== 7. Bijective Generation (対称性の破れと一意生成の厳密化) ===")
(cl-comment "長さ n の行を、長さ k のタイルと長さ 1 のタイルで埋める方法は、")
(cl-comment "「最後のタイルが長さ 1 であるか、長さ k であるか」という排他的かつ網羅的な")
(cl-comment "条件分岐により、重複なく数え上げることが可能である（f(n, k) = f(n-1, k) + f(n-k, k)）。")

(cl-comment "=== 8. Verification against Emptiness (境界値における自己検算) ===")
(cl-comment "n=5, k=2 のとき: f(1)=1, f(2)=2, f(3)=3, f(4)=5, f(5)=8. 8-1=7.")
(cl-comment "n=5, k=3 のとき: f(1)=1, f(2)=1, f(3)=2, f(4)=3, f(5)=4. 4-1=3.")
(cl-comment "n=5, k=4 のとき: f(1)=1, f(2)=1, f(3)=1, f(4)=2, f(5)=3. 3-1=2.")
(cl-comment "合計: 7 + 3 + 2 = 12. 問題文の例と一致することを確認。")

(cl-comment "=== 9. Axiomatic Grounding (公理的定礎と幻覚の超克) ===")
(cl-comment "制約 N=50 に対して、各色の計算量は O(N) であり、全体でも O(3N) である。")
(cl-comment "これはフェルミ推定による 10^7 の壁を遥かに下回る極めて効率的な解法である。")

||#


(defun count-ways-for-color (target-length tile-length)
  "長さ target-length の行を、長さ tile-length のカラータイルと
  長さ 1 のグレータイルで埋める方法の数を動的計画法で計算する。
  少なくとも1つのカラータイルを使う必要があるため、全グレーの 1 通りを最後に引く。"
  (declare (type integer target-length tile-length))
  (let ((dp (make-array (1+ target-length) :element-type 'integer :initial-element 0)))
    ;; 境界条件: 長さ 0 を埋める方法は 1 通り（何も置かない）
    (setf (aref dp 0) 1)
    
    (iterate (for current-len from 1 to target-length)
      ;; 1. 最後がグレータイル(長さ1)の場合
      (setf (aref dp current-len) (aref dp (1- current-len)))
      
      ;; 2. 最後がカラータイル(長さ tile-length)の場合
      (when (>= current-len tile-length)
        (incf (aref dp current-len) (aref dp (- current-len tile-length)))))
    
    ;; 「少なくとも1枚のカラータイルを使用」するため、全てグレー(1通り)を減算
    (let ((result (1- (aref dp target-length))))
      (format t "Color(length=~D): ~D ways~%" tile-length result)
      result)))

(defun solve ()
  "Project Euler P116 を解く。
  長さ 50 の行に対し、赤(2), 緑(3), 青(4) の各タイルについて計算し、その総和を求める。"
  (let* (($target-n 50)
         (red-ways   (count-ways-for-color $target-n 2))
         (green-ways (count-ways-for-color $target-n 3))
         (blue-ways  (count-ways-for-color $target-n 4))
         (total-ways (+ red-ways green-ways blue-ways)))
    (format t "---------------------------~%")
    (format t "Total ways for N=~D: ~D~%" $target-n total-ways)
    total-ways))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Color(length=2): 20365011073 ways
Color(length=3): 122106096 ways
Color(length=4): 5453760 ways
---------------------------
Total ways for N=50: 20492570929

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 65976 bytes
12 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 20492570929
:ok
