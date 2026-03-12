;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0978 (:use cl iterate alexandria))
(in-package #:project-euler-0978)

#||
(cl-text euler-acx-p978

  (cl-comment "=== Project Euler 978: ACX Ontology & SKDT Analysis ===")
  (cl-comment "NMF (Non-Middle Fallacy) Avoidance:")
  (cl-comment "Tracking the full probability distribution of X_t across branches requires tracking O(F_t) states.")
  (cl-comment "This leads to combinatorial explosion. Instead, we perform an ACX Jump (Analytical Reduction)")
  (cl-comment "to track only the linear expectations of the required moments.")

  (forall (t)
    (and (Equal (E_X1 t) 1)
         (Equal (E_X2 t) (Fibonacci t))
         (Equal (E_X3 t) (+ (E_X3 (- t 1)) (* 3 (E_X3 (- t 2)))))))

  (cl-comment "Proof sketch for E_X3:")
  (cl-comment "X_t^3 = X_{t-1}^3 + 3 S_t X_{t-1}^2 |X_{t-2}| + 3 S_t^2 X_{t-1} |X_{t-2}|^2 + S_t^3 |X_{t-2}|^3")
  (cl-comment "Taking expectations and noting E[S_t]=0 isolates the cross term E[X_{t-1} X_{t-2}^2].")
  (cl-comment "Expanding X_{t-1} reveals E[X_{t-1} X_{t-2}^2] = E[X_{t-2}^3], yielding the recurrence.")

  (cl-comment "=== Exact Integer Projection ===")
  (cl-comment "Floating-point exponentiation for Skewness generates illusion (rounding error) due to 53-bit mantissa bounds.")
  (cl-comment "We project the irrational denominator D^(3/2) into pure integer arithmetic.")
  
  (forall (N D K)
    (implies (and (Large K) (Equal Skew (/ N (^ D 1.5))))
             (Equal (Round (* Skew (^ 10 8)))
                    (Round (/ (* N (^ 10 8) (isqrt (* D (^ 10 (* 2 K)))))
                              (* D D (^ 10 K)))))))
)
||#


(defun solve-978 (&optional (n 50))
  "Computes the exact Skewness of X_n rounded to 8 decimal places using ACX Jump and Exact Integer Projection."
  (let ((m2-0 0) (m2-1 1)
        (m3-0 0) (m3-1 1))
    
    ;; Iterate to find M_2(n) = E[X^2] and M_3(n) = E[X^3]
    (iterate (for i from 2 to n)
      (let ((next-m2 (+ m2-1 m2-0))
            (next-m3 (+ m3-1 (* 3 m3-0))))
        (setf m2-0 m2-1
              m2-1 next-m2
              m3-0 m3-1
              m3-1 next-m3)))
              
    (let* ((f-n m2-1)
           (m3-n m3-1)
           
           ;; N = M_3 - 3*M_2 + 2 (since mu = 1)
           (num (+ (- m3-n (* 3 f-n)) 2))
           
           ;; D = M_2 - 1.  We need Skew = num / (D * sqrt(D))
           (d (- f-n 1))
           
           ;; Scale factor for precise integer square root
           ;; D is approx 10^10, so 10^200 provides ~100 decimal digits of precision.
           (k 100)
           (d-scaled (* d (expt 10 (* 2 k))))
           (sqrt-d (isqrt d-scaled))
           
           ;; Compute scaled num and den to directly extract the first 8 decimal places
           (scaled-num (* num sqrt-d (expt 10 8)))
           (scaled-den (* d d (expt 10 k)))
           
           ;; Perform half-up rounding by adding scaled-den/2
           (rounded-skew (floor (+ (* 2 scaled-num) scaled-den)
                                (* 2 scaled-den)))
                                
           (skew-str (format nil "~8,'0D" rounded-skew))
           (len (length skew-str)))
           
      ;; Format integer and fractional parts correctly
      (format nil "~A.~A" 
              (if (= len 8) "0" (subseq skew-str 0 (- len 8)))
              (subseq skew-str (- len 8))))))

;;; ============================================================================
;;; 自己分析 (Self-Analysis)
;;; ============================================================================
;;; 
;;; 1. 現実的な時間での終了可能性について
;;; 本アルゴリズムのループはわずか N = 50 回のみであり、計算は $O(N)$ で完了します。
;;; 最終的な小数部計算においても、内部的な巨大数 (数百桁) に対する `isqrt` (平方根の整数切り捨て) は
;;; Common Lisp の多倍長整数演算により $O(1)$ に近い速度で処理されます。
;;; 無限ループの可能性は皆無であり、実行時間は 1ミリ秒（0.001秒）未満と予想されます。
;;;
;;; 2. LLMが陥りやすい罠
;;; 本問題には、LLMが陥りやすい2つの極めて重大な「悪取空（罠）」が存在します。
;;; (A) 状態追跡の罠: 「確率分布」という言葉に引きずられ、パスの分岐 $2^{t}$ を
;;;     DP やメモ化で保持しようとする非中道の誤謬 (NMF) です。到達可能な状態数は $F_{50}$ まで
;;;     膨張するため、10^10規模の配列が必要となり、実行時間・空間ともに破綻します。
;;; (B) 浮動小数点の幻覚: 歪度の分母にある $\sigma^3 = (F_{50} - 1)^{1.5}$ の計算において、
;;;     標準的な `double-float` (64bit) を用いると、仮数部が 53bit (約15桁) しか存在しないため、
;;;     約 $10^{18}$ にもなる巨大な分子 $M_3(50)$ で割る際に深刻な丸め誤差が生じます。
;;;     要求される「小数点以下8桁の精度」は、浮動小数点を盲信した時点で絶対に得られません。
;;;
;;; 3. 発明や創発、遺伝的アルゴリズムの活用
;;; 本問題は小数点以下8桁の厳密解を要求する数理統計問題であり、評価関数に対する適応度地形
;;; (Fitness Landscape) が全く存在しないため、GA（遺伝的アルゴリズム）の活用は原理的に不可能です。
;;; GAへの執着を早期に棄却することで、純粋な演繹的推論が駆動されました。
;;;
;;; ここでの最大の創発（ACX Jump）は、確率変数の漸化式から期待値の線形性を用いて、
;;; $M_2(t) = \mathbb{E}[X^2]$ が完全に「フィボナッチ数列」に帰着し、
;;; $M_3(t) = \mathbb{E}[X^3]$ がクロスターム $\mathbb{E}[X_{t-1} X_{t-2}^2]$ の計算によって
;;; $M_3(t) = M_3(t-1) + 3 M_3(t-2)$ という驚異的に美しい対称な線形漸化式に昇華されたことです。
;;; さらに、無理数の除算を「10の巨大な冪乗スケールでの有理数近似 (Exact Integer Projection)」に
;;; 投影し、Lispの多倍長整数によって丸め誤差のない真の「勝義諦」としてのコードを現成させました。


#+| Do it | (solve-978 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-978)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.001
Allocation   = 2776 bytes
30 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "254.54470757"
:ok
