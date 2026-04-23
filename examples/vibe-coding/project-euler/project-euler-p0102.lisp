;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-1.5-pro-preview

(cl:in-package cl-user)
(defpackage #:project-euler-0102 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0102)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)

#||

(cl-comment "=== 6. Exact Integer Projection (勝義的整数化による浮動小数点の排除) ===")
(cl-comment "巨大な探索空間において、浮動小数点演算(sqrt, sin等)は丸め誤差という世俗の幻影(Illusion)を生む。")
(cl-comment "不等式や幾何学的条件は、必ず両辺を二乗・分母払いし、純粋な整数演算(isqrt, floor等)へ還元(Projection)すること。")

(cl-comment "=== 9. Axiomatic Grounding (公理的定礎と幻覚の超克) ===")
(cl-comment "不完全な情報や類似問題からの帰納的推測による跳躍は、LLM特有の悪取空(Hallucination)を生む。")
(cl-comment "直感的なO(1)への還元を急ぐ前に、必ず問題の正確なルール(公理)を完全に受容し、演繹的に矛盾がない構造を定礎すること。")


(cl-comment "
### ナップザック・プロトコル分析

1. **問題の特定**:
   1000個の三角形 $(A, B, C)$ について、原点 $(0,0)$ がその内部に含まれるか判定する。座標は $[-1000, 1000]$ の整数。

2. **数論的ショートカット（幾何学的不変量）**:
   点 $P$ が三角形 $ABC$ の内部にあるための必要十分条件は、三角形の各辺 $AB, BC, CA$ に対して点 $P$ が常に同じ側（左側または右側）にあることである。
   原点 $O(0,0)$ に対して、ベクトル $\vec{OA}, \vec{OB}, \vec{OC}$ を定義すると、2次元外積（Cross Product）の符号によって向きを判定できる。
   - $S_1 = \text{sign}(\vec{OA} \times \vec{OB}) = \text{sign}(x_A y_B - x_B y_A)$
   - $S_2 = \text{sign}(\vec{OB} \times \vec{OC}) = \text{sign}(x_B y_C - x_C y_B)$
   - $S_3 = \text{sign}(\vec{OC} \times \vec{OA}) = \text{sign}(x_C y_A - x_A y_C)$
   これら3つの符号がすべて一致する場合、原点は内部に存在する。

3. **フェルミ推定のレッドライン**:
   - データ数 $N = 1000$。
   - 1三角形あたりの演算: 整数乗算 6回、減算 3回、比較数回。
   - 総演算回数 $\approx 10^4$。
   - $10^7$ の壁に対して極めて余裕があり、Common Lispの整数演算（fixnum）で瞬時に（マイクロ秒単位で）完了する。

4. **計算の「対称性」と「正確性」**:
   - 座標が整数のため、外積の結果も必ず整数となる。浮動小数点数（`double-float`）を一切排除し、`Exact Integer Arithmetic`（勝義的整数化）のみで判定を行うことで、丸め誤差による誤判定を完全に防ぐ。
   - 三角形の頂点順序（時計回り/反時計回り）に依存しないよう、符号が「すべて正」または「すべて負」であることをチェックする。

5. **制約の特定**:
   - 座標最大値 1000。
   - 外積最大値 $\approx 1000 \times 1000 - (-1000 \times 1000) = 2 \times 10^6$。
   - LispWorksの `fixnum` (60bit) に十分収まるため、オーバーフローの懸念はない。
")
||#

(defun origin-inside-p (x1 y1 x2 y2 x3 y3)
  "原点が三角形内部にあるか外積の符号一致で判定する。
   浮動小数点を使用せず、整数演算のみで完結させる（Exact Integer Projection）。"
  (let ((cp1 (- (* x1 y2) (* x2 y1)))
        (cp2 (- (* x2 y3) (* x3 y2)))
        (cp3 (- (* x3 y1) (* x1 y3))))
    ;; すべての符号が一致しているか (すべて正、またはすべて負)
    ;; 境界上のケース(cp=0)は問題文の『三角形を形成する』という条件から内部ではないと判断できるが、
    ;; 厳密には (and (>= cp1 0) (>= cp2 0) (>= cp3 0)) 等の考慮が必要。
    ;; Project Eulerのこの問題では、原点が辺上にあることは想定されていない。
    (or (and (> cp1 0) (> cp2 0) (> cp3 0))
        (and (< cp1 0) (< cp2 0) (< cp3 0)))))

(defun parse-csv-line (line)
  "カンマ区切りの文字列を整数リストに変換する。"
  (let ((start 0))
    (iterate
      (for pos = (position #\, line :start start))
      (collect (parse-integer (subseq line start pos)) into coords)
      (while pos)
      (setf start (1+ pos))
      (finally (return coords)))))

(defun solve (&optional (filename "/tmp/0102_triangles.txt"))
  "Project Euler 102 を解く。ファイルから三角形データを読み込み、原点を包含する数をカウントする。"
  (format t "Reading data from ~A...~%" filename)
  (unless (probe-file filename)
    (error "File ~A not found. Please ensure the data file is in the current directory." filename))
  
  (let ((count 0)
        (line-idx 0))
    (with-open-file (in filename)
      (iterate
        (for line = (read-line in nil))
        (while line)
        (incf line-idx)
        (let ((coords (parse-csv-line line)))
          (destructuring-bind (x1 y1 x2 y2 x3 y3) coords
            (when (origin-inside-p x1 y1 x2 y2 x3 y3)
              (incf count)
              ;; 最初の2つの例を確認するためのログ
              (when (<= line-idx 2)
                (format t "Example line ~D: Origin is INSIDE.~%" line-idx))))
          ;; 例2が外側であることを確認
          (when (and (= line-idx 2) (not (origin-inside-p (first coords) (second coords) (third coords) 
                                                         (fourth coords) (fifth coords) (sixth coords))))
            (format t "Example line 2: Origin is OUTSIDE.~%")))))
    
    (format t "Total triangles containing the origin: ~D~%" count)
    count))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Reading data from /tmp/0102_triangles.txt...
Example line 1: Origin is INSIDE.
Example line 2: Origin is OUTSIDE.
Total triangles containing the origin: 228

User time    =        0.003
System time  =        0.000
Elapsed time =        0.002
Allocation   = 533744 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 228
:ok
