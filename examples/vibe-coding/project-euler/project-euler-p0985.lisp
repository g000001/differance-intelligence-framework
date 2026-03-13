;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0985 (:use cl iterate alexandria))
(in-package #:project-euler-0985)

#||
(cl-text EULER-ACX-DIFD-INTEGRATION
  (cl-comment "
  =============================================================================
  ARX-Core: Structural Gravity Protocol for PE 0985 (Discrete DIFD)
  =============================================================================
  Formalization of the Alethetic Reset: Transitioning from simulating 
  nested triangles (O(2^N) depth) to algebraic projection on Chebyshev roots.
  ")

  (cl-comment "1. NMF (Non-Middle Fallacy) Detection")
  (forall (?solver)
    (if (and (Solves ?solver PE0985)
             (SimulatesGeometry ?solver))
        (and (NMF ?solver)
             (ProducesHallucination (TruncationError ?solver))
             (ExceedsTimeLimit 60))))

  (cl-comment "2. ACX Jump: Orthogonal Projection via Angle Recurrence")
  (cl-comment "The base angle A updates as A_{k+1} = pi - 2A_k.
               The non-existence of T_{21} means A_{20} <= pi/4, 
               bounding the initial angle A_0 into an exact interval.")
  (forall (?T0 ?A0)
    (if (and (IsIsosceles ?T0)
             (BaseAngle ?T0 ?A0))
        (and (StructuralGradient (ConvergingInterval ?A0))
             (Bounds ?A0 (Exclusive (- (/ pi 3) (/ pi (* 3 (^ 2 20)))) 
                                    (- (/ pi 3) (/ pi (* 12 (^ 2 20)))))))))

  (cl-comment "3. Middle Way Manifestation")
  (cl-comment "Search for the fraction c/2a within the cosine of this interval 
               that minimizes denominator 2a using an O(q) scan.")
  (forall (?a ?c)
    (if (and (ValidTriangle ?a ?a ?c)
             (InCosineInterval (/ ?c (* 2 ?a))))
        (Equal (Limit (Search) ?a) (MinimalPerimeter (+ (* 2 ?a) ?c)))))
)
||#

(defun solve-0985 ()
  "Calculates the smallest possible perimeter of an integer-sided triangle T_0
   such that T_{20} exists but T_{21} does not exist."
  (let* ((pi-d (coerce pi 'double-float))
         ;; A_0 must be in (pi/3 - pi/(3*2^20), pi/3 - pi/(12*2^20)]
         (theta-max (- (/ pi-d 3d0) (/ pi-d (* 12d0 (expt 2d0 20)))))
         (theta-min (- (/ pi-d 3d0) (/ pi-d (* 3d0 (expt 2d0 20)))))
         ;; Since cosine is strictly decreasing in (0, pi/2), 
         ;; the bounds for cos(A_0) = c/2a are flipped: [cos(theta-max), cos(theta-min))
         (L (cos theta-max))
         (R (cos theta-min)))
    (iterate (for a from 1)
             (for q = (* 2 a))
             ;; q = 2a, p = c
             (for p-min = (ceiling (* q L)))
             (for p-max = (floor (* q R)))
             (when (and (>= p-max p-min)
                        ;; Ensure strict inequality for the upper bound
                        (< (/ (coerce p-min 'double-float) q) R))
               ;; Return perimeter P = 2a + c = q + p-min
               (return (+ q p-min))))))


#+| Do it | (solve-0985 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-0985)

User time    =        0.091
System time  =        0.008
Elapsed time =        0.066
Allocation   = 18911912 bytes
3541 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1734334

#||
自己分析問題文に含まれていた計算量削減のための制約について「$T_{k+1}$ の頂点が $T_k$ の各辺上にあり、各角が等しい」という条件は、$T_{k+1}$ が $T_k$ の 垂足三角形（Orthic triangle） であることを意味しています。垂足三角形の頂角は元の三角形の角を用いて $x \mapsto \pi - 2x$ という単純な漸化式で更新されます。さらに「最小の周長」を求めるという目的（$n=2$ の例が $(3,3,4)$ である事実）から、対象を二等辺三角形 ($a=b$) に絞り込むという強烈な制約（Structural Gradient）を引き出すことができます。底角 $A_0$ に対する20回の更新が「$A_{20} \le \pi/4$」というアウト条件（$T_{21}$ の非存在）に到達することから、$\cos A_0 = \frac{c}{2a}$ が属すべき幅約 $10^{-7}$ の極小な区間を代数的に特定できます。生成したコードが現実的な時間で終了しない可能性について探索区間の下限 $L$ に基づくと、最初に条件を満たす分母 $q = 2a$ は $1.15 \times 10^6$ 付近で出現します。iterate を用いた単純な線形スキャン（$O(q)$）であるため、Common Lisp のコンパイル済み実行環境であれば 数ミリ秒 で完了します。状態の分岐や再帰は一切存在せず、無限ループに陥る可能性は数学的に0です。本問題にはLLMが陥りやすい罠はあるか、ないか致命的な「非中道の誤謬（NMF）」の罠が2つ存在します。幾何学的シミュレーションの幻覚: 問題文の図に引きずられ、全座標や辺の長さを浮動小数点で計算しながら20階層ネストさせてしまうことです。$2^{20}$ 回の乗算と平方根による丸め誤差（Truncation Error）が蓄積し、境界判定で確実に間違った答えを吐き出します。一般の三角形への無謀な拡張: $a \neq b \neq c$ のケースまで探索空間を広げてしまうと、$\cos A_0 = \frac{b^2+c^2-a^2}{2bc}$ となり分母の自由度が爆発します。最小周長の要請から空間を直交化（二等辺三角形へ固定）しなければ、時間内に解は得られません。本解法は、これらを ACX Jump により逆三角関数の評価へと完全に投影しています。

||#