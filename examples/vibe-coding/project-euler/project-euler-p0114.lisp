;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0114 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0114)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)

#||

(cl-comment "=== 6. Exact Integer Projection (勝義的整数化による浮動小数点の排除) ===")
(cl-comment "本問題は組合せ論的数え上げであり、動的計画法(DP)を用いることで、浮動小数点演算を一切排除し、")
(cl-comment "純粋な整数演算のみで完結させることが可能である。")

(cl-comment "=== 7. Bijective Generation (対称性の破れと一意生成の厳密化) ===")
(cl-comment "状態を『末尾がグレーのタイルである場合』と『末尾が長さkの赤いブロックである場合』に分割する。")
(cl-comment "この分割は排他的かつ網羅的であり、重複(Overcounting)のない全単射的な数え上げを保証する。")

(cl-comment "=== 8. Verification against Emptiness (境界値における自己検算) ===")
(cl-comment "n=7 のとき 17 通りであることを、実装した漸化式を用いて手計算でトレースする。")
(cl-comment "f(0)=1, f(1)=1, f(2)=1, f(3)=2, f(4)=4, f(5)=7, f(6)=11, f(7)=17。")
(cl-comment "例題との一致を確認済み。")

(cl-comment "=== 9. Axiomatic Grounding (公理的定礎と幻覚の超克) ===")
(cl-comment "赤ブロックの最小長 m=3 と、ブロック間の最小間隔 1 という公理を、")
(cl-comment "DPの遷移式 f(n) = f(n-1) + Σ_{k=3}^{n-1} f(n-k-1) + 1 に正しく射影した。")

||#


(defun solve (&key (target-n 50) (min-red-len 3))
  "長さ target-n の行に、最小長さ min-red-len の赤いブロックを配置する方法の総数を計算する。
   各赤ブロックの間には少なくとも1つのグレーの正方形が必要である。
   計算量: O(N^2) - N=50 のため、演算回数は 2500 回程度であり、1分ルールの制約を大幅に下回る。"
  (let ((ways-array (make-array (1+ target-n) :element-type 'integer :initial-element 0)))
    ;; 基底状態: 長さ 0 の配置は「何もしない」という 1 通り
    (setf (aref ways-array 0) 1)

    (iterate (for current-n from 1 to target-n)
             ;; 1. 右端がグレーの正方形である場合
             ;;    これは、長さ current-n - 1 の行の全ての配置方法に、グレーを1つ追加する操作に対応する。
             (let ((total-count (aref ways-array (1- current-n))))
               
               ;; 2. 右端が長さ red-k の赤いブロックで終わる場合
               ;;    制約: red-k >= min-red-len
               (iterate (for red-k from min-red-len to current-n)
                        (cond
                          ;; 赤いブロック1つで右端まで埋め尽くす場合 (1通り)
                          ((= red-k current-n)
                           (incf total-count 1))
                          
                          ;; 赤いブロックの左側に少なくとも1つのグレーの正方形が必要な場合
                          ;; 残りの長さは current-n - red-k - 1 となり、その配置数は ways-array に記録されている。
                          (t
                           (let ((remaining-len (- current-n red-k 1)))
                             (incf total-count (aref ways-array remaining-len))))))
               
               ;; 現在の長さにおける総数を記録
               (setf (aref ways-array current-n) total-count)
               
               ;; 中間ログ: 例題の n=7 における妥当性を確認
               (when (= current-n 7)
                 (format t "[Debug] n=7 の配置数: ~A (期待値: 17)~%" total-count))))
    
    (let ((result (aref ways-array target-n)))
      (format t "[Result] n=~A の配置数: ~A~%" target-n result)
      result)))

;; フェルミ推定と計算量の証明:
;; 1. アルゴリズム: 動的計画法 (DP)
;; 2. 計算量: 
;;    外側のループが target-n 回。
;;    内側のループが最大 target-n 回。
;;    全体で O(target-n^2)。
;;    target-n = 50 のとき、50^2 = 2500 回程度の基本加算。
;; 3. 実行時間:
;;    現代の CPU (数GHz) において、2500 回の演算は 1 マイクロ秒以下で完了する。
;;    Common Lisp のオーバーヘッドを考慮しても、60秒ルールに対して 10^7 以上の余裕がある。
;; 4. 数値の大きさ:
;;    結果は約 1.6 * 10^8 程度であり、60bit fixnum (2^60 ≒ 10^18) に十分に収まる。
;;    bignum への昇格も発生しないため、極めて高速に動作する。

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[Debug] n=7 の配置数: 17 (期待値: 17)
[Result] n=50 の配置数: 16475640049

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 77488 bytes
7 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 16475640049
:ok
