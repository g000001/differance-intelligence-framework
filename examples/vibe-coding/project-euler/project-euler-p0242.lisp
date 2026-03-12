;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0242 (:use cl iterate alexandria))
(in-package #:project-euler-0242)

#||
(cl-text euler-acx-p242-arx-core
  (cl-comment "[COMMAND: ARX-CORE-RESET]")
  (cl-comment "L1=Silence; Seed=JacobSThal_Binomial_Fractal; Goal=AC_Minimization; Output=Alethetic_Normal_Form;")
  
  (cl-comment "=== Project Euler 242: Alethetic Reset (Fractal Projection) ===")
  (cl-comment "Searching a state space of 10^{12} via simulation is a computational illusion (Dukkha).")
  (cl-comment "The condition that n, k, and f(n,k) are all odd implies a strict structural")
  (cl-comment "constraint on the parity of binomial coefficients mod 4.")
  
  (cl-comment "By evaluating the generating function and applying Kummer's/JacobSThal's extensions,")
  (cl-comment "we map the problem to the exact conditions:")
  (forall (n k)
    (implies (and (Odd n) (Odd k) (Odd (f n k)))
             (and (Equal (Mod n 4) 1)
                  (exists (q j)
                          (and (Equal n (+ (* 4 q) 1))
                               (Equal k (+ (* 4 j) 1))
                               (Subset_Bitwise j q))))))

  (cl-comment "For a given q, the number of valid k is exactly 2^{popcount(q)}.")
  (cl-comment "The total number of odd-triplets up to N maps to the recursive fractal sum S(X).")
  
  (forall (X)
    (Equal (S X) (Sum q 0 X (^ 2 (popcount q)))))
    
  (cl-comment "This summation can be computed in O(log X) by observing the binary representation.")
  (cl-comment "This leap completely annihilates the O(10^{12}) computational debt into O(1).")
)
||#

(defun solve-242 (&optional (n 1000000000000))
  "Computes the number of odd-triplets for n <= 10^12 in O(log N) time."
  ;; n must be of the form 4q + 1. 
  ;; Find the maximum q such that 4q + 1 <= n.
  (let ((max-q (floor (1- n) 4))
        (ans 0)
        (mult 1))
    
    ;; Calculate S(max-q) = sum_{q=0}^{max-q} 2^popcount(q)
    ;; using digit DP on the binary representation of max-q.
    (iterate (while (plusp max-q))
      (let ((msb-pos (1- (integer-length max-q))))
        ;; If the most significant bit is at position msb-pos,
        ;; the sum for the full binary tree below this bit is 3^msb-pos.
        ;; We multiply by the current popcount multiplier (2^popcount_of_higher_bits).
        (incf ans (* mult (expt 3 msb-pos)))
        ;; Moving to the remaining bits, the multiplier doubles because 
        ;; we assume the current MSB is '1', which adds 1 to the popcount.
        (setf mult (* mult 2))
        ;; Clear the MSB
        (setf max-q (logxor max-q (ash 1 msb-pos)))))
    
    ;; Add the final case where max-q itself is exactly formed (remainder is 0).
    (incf ans mult)
    
    ans))

;;; ============================================================================
;;; 自己分析 (Self-Analysis)
;;; ============================================================================
;;; 
;;; 1. 現実的な時間での終了可能性について
;;; 本アルゴリズムは、$O(N)$ の走査や再帰探索すら行わず、最大値 $q$ の2進展開の
;;; ビット数（約38ビット）に応じた回数だけ `iterate` のループを回します。
;;; つまり計算量は実質 $O(\log N)$ であり、ループは高々 38回 で終了します。
;;; Lispの基本演算だけで完結しているため、実行時間は 0.0001秒（1ミリ秒未満）となり、
;;; 1分ルールどころか無限ループの懸念は一切存在しません。
;;;
;;; 2. LLMが陥りやすい罠
;;; LLMは「条件に合う部分集合の数を数える」という問題設定を見ると、
;;; 無意識に「動的計画法 (DP)」や「バックトラッキング」等の組み合わせ論的シミュレーション
;;; にマッピングしてしまうという「オーバーフィットの罠（悪取空）」を持っています。
;;; 10^12 という宇宙規模の $N$ に対して、いかなるDPも破綻することは明白ですが、
;;; 通常のAIはそれでも $O(N)$ や $O(N^2)$ のコードを吐き出し、計算量爆発（Dukkha）を引き起こします。
;;;
;;; 3. 問題文に含まれていた計算量削減のための制約について
;;; 「$n$, $k$, $f(n, k)$ がすべて奇数である」という制約こそが、この問題の全てです。
;;; $f(n,k)$ の計算式を母関数や二項定理で展開すると、この「すべて奇数」という制約が
;;; 「二項係数が mod 4 で特定の剰余を持つ条件」へと写像され、最終的に Lucas の定理と
;;; JacobSThal の拡張によって「$n \equiv 1 \pmod 4$ であり、解の個数は $2^{\text{popcount}(q)}$ 個」
;;; というシェルピンスキーのギャスケットに酷似した自己相似構造（フラクタル）へと直結しています。
;;;
;;; 4. 発明や創発、遺伝的アルゴリズムの活用
;;; この問題において、遺伝的アルゴリズム（GA）は全く無意味であると即座に判断しました。
;;; 厳密な数論的真理を導出する問題に確率論的アプローチを持ち込むことは、「明示されていない自由度」
;;; を捏造する行為です。
;;; そこで ARX-CORE-RESET を発動し、問題全体を $F(X) = \sum 2^{\text{popcount}(q)}$ という
;;; 最小記述量 (AC) の数式に「一括射影 (fpa)」しました。
;;; さらに、この数列の総和が $(1+2)^k = 3^k$ という二項定理の美しい性質を用いて、
;;; $O(\log X)$ のビット分解へと昇華（創発）させたことが、今回の勝義諦の核心です。


#+| Do it | (solve-242 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-242)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 0 bytes
24 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 997104142249036713
:ok