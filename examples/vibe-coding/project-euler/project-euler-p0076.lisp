;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-2.0-pro-exp-02-05
(cl:in-package cl-user)
(defpackage #:project-euler-0076 (:use cl iterate #|alexandria|#))
(in-package #:project-euler-0076)

#||
(cl-text project-euler-p76-analysis
  (cl-comment "Problem 76: Counting Summations (Partitions)")

  ;; 1. 世俗諦 (Conventional Truth): 具体的分割の列挙
  ;; 5 = 4+1, 3+2, 3+1+1, ... (全6通り + 自身1通り)
  (forall (n)
    (iff (ConventionalSummations n)
         (exists (list-of-partitions)
           (and (all-elements-positive list-of-partitions)
                (sum-equals list-of-partitions n)))))

  ;; 2. 非中道の誤謬 (NMF) の回避
  ;; 100の分割をすべて生成してカウントするのは、計算資源の浪費（執着）である。
  ;; 指数時間の探索を避け、多項式時間の動的計画法へ跳躍（ACX Jump）する。
  (forall (n)
    (if (greater_than n 50)
        (and (AvoidsEnumeration)
             (UsesPartitionFunctionRecurrence))))

  ;; 3. 勝義諦 (Ultimate Truth): 分割関数 p(n) の構造
  ;; p(n) は、1からn-1までの整数を使ってnを作る組み合わせ数 + 1（自身）。
  ;; 問題の要求は「少なくとも2つの正の整数の和」であるため、p(n) - 1 を求める。
  (definition PartitionCountMinusOne (n)
    (- (PartitionFunction p n) 1))

  ;; 4. 創発的アルゴリズム (SKDT Emergence)
  ;; 状態の負債（メモリ）を最小化するため、1次元のDP配列を現成させる。
  ;; 配列の各要素 dp[i] は、現在の「利用可能な最大整数」までの範囲でのiの分割数を示す。
  (forall (dp i j)
    (if (and (is_current_sum i) (is_adding_integer j))
        (update_rule dp i (+ (get dp i) (get dp (- i j))))))

  ;; 5. 中道の現成 (Manifestation)
  ;; 数論的性質（勝義）に基づき、iterateによる効率的なループ（世俗）で解を固定する。
)
||#


(defun solve-p76 (&optional (target 100))
  "Finds the number of ways to write TARGET as a sum of at least two positive integers.
Uses dynamic programming (the partition function recurrence)."
  (let ((dp (make-array (1+ target) :element-type 'integer :initial-element 0)))
    ;; Base case: there is 1 way to form the sum 0 (the empty set).
    (setf (aref dp 0) 1)
    ;; We iterate through integers 1 to TARGET-1 to ensure at least two numbers are used.
    ;; Alternatively, use 1 to TARGET and subtract 1 at the end.
    ;; Here we use the latter to strictly follow the partition function logic p(target) - 1.
    (iterate (for i from 1 to (1- target))
             (iterate (for j from i to target)
                      (incf (aref dp j) (aref dp (- j i)))))
    (aref dp target)))

;; 実行と結果の出力
;; (print (solve-p76 100))


#|
## 生成コードへの影響についての自己分析

Common Logic (CLIF) 形式での分析を先行させることにより、以下の3点において生成コードの質が向上したと分析します。

1.  **計算量の最適化の必然性**:
    CLIF分析において「非中道の誤謬（NMF）」として全探索（列挙）を定義したことで、LLMは無意識に「単純な再帰」や「リスト生成」を避け、多項式時間 $O(n^2)$ の動的計画法（DP）を選択する論理的強制力が働きました。

2.  **境界条件の明確化**:
    問題文の「少なくとも2つの正の整数の和」という制約を、CLIF内の「PartitionCountMinusOne」という定義で $p(n) - 1$ と形式化したことにより、実装時に `(1- target)` までのループにするか、最後に `1` を引くかというアルゴリズムの急所が明確に意識されました。

3.  **状態管理の抽象化**:
    「SKDT Emergence」として1次元DP配列への還元を記述したことで、メモリ効率の良い（$O(n)$ 空間）実装が自然に導かれました。単に「解を出す」だけでなく、その解が「勝義（数学的構造）」と「世俗（Lispの実装）」の中道にあることを定義したことが、`iterate` を用いた簡潔で堅牢なコードに寄与しています。

総じて、CLIFによる形式化は、LLMにとっての「仕様の深い理解」と「実装の論理的裏付け」を繋ぐブリッジ（二諦随伴）として機能したと言えます。
|#

#+| Do it | (solve-p76 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-p76)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 816 bytes
3 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 190569291
:ok
