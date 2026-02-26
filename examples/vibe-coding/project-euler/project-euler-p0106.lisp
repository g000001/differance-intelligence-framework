;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
;;; <p>Let $S(A)$ represent the sum of elements in set $A$ of size $n$. We shall call it a special sum set if for any two non-empty disjoint subsets, $B$ and $C$, the following properties are true:</p>
;;; <ol><li>$S(B) \ne S(C)$; that is, sums of subsets cannot be equal.</li>
;;; <li>If $B$ contains more elements than $C$ then $S(B) \gt S(C)$.</li>
;;; </ol><p>For this problem we shall assume that a given set contains $n$ strictly increasing elements and it already satisfies the second rule.</p>
;;; <p>Surprisingly, out of the $25$ possible subset pairs that can be obtained from a set for which $n = 4$, only $1$ of these pairs need to be tested for equality (first rule). Similarly, when $n = 7$, only $70$ out of the $966$ subset pairs need to be tested.</p>
;;; <p>For $n = 12$, how many of the $261625$ subset pairs that can be obtained need to be tested for equality?</p>
;;; <p class="smaller">NOTE: This problem is related to <a href="problem=103">Problem 103</a> and <a href="problem=105">Problem 105</a>.</p>

(cl:in-package cl-user)
(defpackage #:project-euler-0106
  (:use #:cl #:iterate)
  (:import-from #:alexandria #:binomial-coefficient))
(in-package #:project-euler-0106)

;;; ============================================================================
;;; 二諦随伴 (Two-Truths Entanglement) による問題解決
;;; ----------------------------------------------------------------------------
;;; 世俗諦 (Conventional Truth): 261,625個の全部分集合ペアを生成し、支配関係を検証する。
;;; 勝義諦 (Ultimate Truth): 組合せ論的還元により、サイズが等しく、かつ
;;;                         カタラン数的構造（要素ごとの優越）を持たないペアのみを数え上げる。
;;; 中道 (Middle Way): 以下の最適化された計算コードとして現成する。
;;; ============================================================================

(defun catalan-number (n)
  "第nカタラン数を計算する。 C_n = 1/(n+1) * (2n choose n)"
  (/ (binomial-coefficient (* 2 n) n)
     (1+ n)))

(defun count-unnecessary-tests (k)
  "サイズkの2つの互いに素な集合において、要素ごとの大小関係が一方向（支配的）であるペアの数。
   これは、2k個の要素からk個を選ぶ際、カタラン数の条件を満たすものに相当する。"
  (catalan-number k))

(defun solve-euler-106 (n)
  "n要素の集合における、検証が必要な部分集合ペアの総数を求める。
   Rule 2が既に満たされているため、サイズが異なるペアは検証不要。
   サイズk（2 <= k <= n/2）が等しいペアにおいて、要素ごとの優越関係がないものをカウントする。"
  (iter (for k from 2 to (floor n 2))
    ;; 1. n個から2k個の要素を選ぶ: (n choose 2k)
    ;; 2. 選んだ2k個をサイズkずつの2つの集合 B, C に分ける方法は 1/2 * (2k choose k)
    ;; 3. そのうち、一方の要素が常に他方より小さい（検証不要な）ケースは catalan-number(k) 通り。
    ;; 4. したがって、検証が必要な数は { 1/2 * (2k choose k) } - catalan-number(k)
    (let* ((choose-2k (binomial-coefficient n (* 2 k)))
           (half-combinations (/ (binomial-coefficient (* 2 k) k) 2))
           (guaranteed-ordered (catalan-number k))
           (needed-per-2k (- half-combinations guaranteed-ordered)))
      (sum (* choose-2k needed-per-2k)))))

(defun main ()
  "n=12の場合の解を計算し出力する。"
  (let ((result (solve-euler-106 12)))
    (format t "Project Euler 106 Result: ~A~%" result)
    result))

;; 計算の実行
;(main)

;;; ============================================================================
;;; 自己分析：二諦随伴プロトコルの貢献
;;; ----------------------------------------------------------------------------
;;; 1. NMF (非中道の誤謬) の回避:
;;;    問題が提示する「261,625個のペア」という世俗的（Conventional）な数字に執着し、
;;;    愚直な全探索（O(2^N)空間の走査）を実装することは、計算資源の無駄遣いであり、
;;;    「執着」による誤謬です。本実装では、数論的還元（勝義諦）を用いることで、
;;;    ループ回数を n/2 回という極小の探索空間（O(N)）へと「跳躍（ACX Jump）」させました。
;;;
;;; 2. 差延（Différance）としてのカタラン数:
;;;    「検証が必要なペア」と「検証が不要なペア」の境界線は、要素間の順序構造にあります。
;;;    カタラン数は、この「順序が逆転しない（＝境界を越えない）」構造を象徴する
;;;    数学的不動点（Dfix0）として機能しています。
;;;    (1/2 * (2k choose k) - C_k) という式は、全可能性から安定的な構造（空性）を
;;;    差し引いた「残差」を抽出しており、これがまさに「中道」における現成です。
;;;
;;; 3. Exact Integer Projection (勝義的整数化):
;;;    浮動小数点によるカタラン数計算を避け、`alexandria:binomial-coefficient` を用いた
;;;    純粋な整数演算に徹することで、丸め誤差という「世俗の幻影」を排除しました。
;;;    これにより、n=12という具体的な制約において、絶対的な確信（Aletheic certainty）を
;;;    持って解を導出することが可能となりました。
;;; ============================================================================

#+| Do it | (main )
#|------------------------------------------------------------|
Timing the evaluation of (main)
Project Euler 106 Result: 21384

User time    =        0.010
System time  =        0.002
Elapsed time =        0.028
Allocation   = 21128 bytes
919 Page faults
GC time      =        0.000
→ 21384
 |------------------------------------------------------------|#

