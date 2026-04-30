;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0109 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0109)

#||
(cl-comment "=== ナップザック・プロトコルに基づく分析 ===")
(cl-comment "1. 不変量と数論的構造:")
(cl-comment "   ダーツのチェックアウトは (Dart1, Dart2, Dart3) の3つ組で構成される。")
(cl-comment "   制約1: Dart3 は必ず『ダブル』である。")
(cl-comment "   制約2: {Dart1, Dart2} は順序を区別しない『組み合わせ』である（0点、つまり投げない場合を含む）。")
(cl-comment "   制約3: 各ダーツが当たる『領域』は、たとえ同じスコアであっても区別される（例: S2とD1は共に2点だが別領域）。")
(cl-comment "   全領域数: シングル(1-20, 25) = 21, ダブル(1-20, 25) = 21, トリプル(1-20) = 20。合計62領域。")

(cl-comment "2. 計算量評価:")
(cl-comment "   最後の一投(Dart3)の選択肢: 21通り。")
(cl-comment "   残り2投の選択肢(0点を含む): 62 + 1 = 63種類。")
(cl-comment "   順序を無視した2投の組み合わせ数: H(63, 2) = (63 * 64) / 2 = 2016通り。")
(cl-comment "   総探索空間: 21 * 2016 = 42,336通り。")
(cl-comment "   フェルミ推定: 4.2 * 10^4 回の演算。10^7の壁に対して極めて余裕があり、O(1)に近い速度で完結する。")

(cl-comment "3. 数論的ショートカット:")
(cl-comment "   - 対称性の利用: Dart1とDart2をインデックス管理し i <= j とすることで重複計算を排除。")
(cl-comment "   - 状態の事前定義: 領域のスコアをリスト化し、動的な計算を最小化。")


||#


(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)

(defun solve ()
  "Project Euler P109: 100未満のチェックアウト総数を求める"
  (let* ((single-scores (append (iterate (for i from 1 to 20) (collect i)) (list 25)))
         (double-scores (append (iterate (for i from 1 to 20) (collect (* i 2))) (list 50)))
         (treble-scores (iterate (for i from 1 to 20) (collect (* i 3))))
         ;; 全ての「得点が入る領域」のスコアリスト (62要素)
         (all-region-scores (concatenate 'vector single-scores double-scores treble-scores))
         ;; チェックアウトの最後の一投になれる「ダブル」のスコアリスト (21要素)
         (checkout-doubles (coerce double-scores 'vector))
         (num-all (length all-region-scores))
         (target-limit 100)
         (total-ways-all-time 0)   ; 問題文の検証用 (42336になるはず)
         (total-ways-under-limit 0)) ; 本題の回答用

    (format t "Starting calculation for Project Euler 109...~%")
    (format t "Number of regions: ~D~%" num-all)
    (format t "Number of possible finishing doubles: ~D~%" (length checkout-doubles))

    ;; 3投目を固定してループ
    (iterate (for d3-score in-vector checkout-doubles)
      ;; 1投目と2投目の組み合わせ（投げない場合=スコア0を含む）
      ;; i = -1 は「1投目を投げない」ことを意味する
      (iterate (for i from -1 to (1- num-all))
        (iterate (for j from i to (1- num-all))
          (let* ((s1 (if (minusp i) 0 (aref all-region-scores i)))
                 (s2 (if (minusp j) 0 (aref all-region-scores j)))
                 (total-score (+ s1 s2 d3-score)))
            
            ;; 全てのチェックアウトをカウント
            (incf total-ways-all-time)
            
            ;; 100未満のチェックアウトをカウント
            (when (< total-score target-limit)
              (incf total-ways-under-limit))))))

    ;; 中間ログと最終結果
    (format t "--- Verification ---~%")
    (format t "Total distinct ways calculated: ~D~%" total-ways-all-time)
    (format t "Expected total (from problem): 42336~%")
    (if (= total-ways-all-time 42336)
        (format t "Verification SUCCESS: Total count matches problem description.~%")
        (format t "Verification FAILURE: Count mismatch.~%"))

    (format t "--- Result ---~%")
    (format t "Total ways with score less than 100: ~D~%" total-ways-under-limit)
    
    total-ways-under-limit))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting calculation for Project Euler 109...
Number of regions: 62
Number of possible finishing doubles: 21
--- Verification ---
Total distinct ways calculated: 42336
Expected total (from problem): 42336
Verification SUCCESS: Total count matches problem description.
--- Result ---
Total ways with score less than 100: 38182

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 12792 bytes
10 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 38182
:ok