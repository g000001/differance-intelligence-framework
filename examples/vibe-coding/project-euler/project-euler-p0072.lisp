
;;; (cl-text

;;; ; ==============================================================================
;;; ; Project Euler Problem 72: Counting fractions
;;; ; ------------------------------------------------------------------------------
;;; ; 二諦随伴プロトコルに基づく解答
;;; ; ------------------------------------------------------------------------------
;;; ; この問題は、d <= 1,000,000 の範囲で、分子 n と分母 d が正の整数であり、
;;; ; n < d かつ HCF(n,d)=1 を満たす既約真分数の数を数えるものです。
;;; ;
;;; ; 問題の分析：
;;; ; 特定の分母 d について、条件 (n < d かつ HCF(n,d)=1) を満たす分子 n の数は、
;;; ; オイラーのトーシェント関数 φ(d) に等しい。
;;; ; したがって、求める総数は Σ φ(d) (d=2 から D まで) となる。
;;; ; (d=1 の場合、n < 1 を満たす正の整数 n は存在しないため、d=2 から開始する。)
;;; ;
;;; ; アルゴリズム：
;;; ; 1. φ(d) の値を D までのすべての d について計算するために、篩（Sieve）を用いる。
;;; ;    - 配列 phi を D+1 のサイズで初期化し、phi[i] = i とする。
;;; ;    - p を 2 から D まで順に走査する。
;;; ;    - もし phi[p] がまだ p のままであれば、p は素数である。
;;; ;    - p のすべての倍数 j (p, 2p, 3p, ...) について、phi[j] を phi[j] - phi[j]/p で更新する。
;;; ; 2. 計算された φ(d) の値を d=2 から D まで合計する。
;;; ;
;;; ; 計算量：
;;; ; 篩のステップは O(D log log D) または O(D log D) の計算量を持つ。
;;; ; 合計のステップは O(D) の計算量を持つ。
;;; ; D = 1,000,000 の場合、この計算量は約 2 × 10^7 オペレーションとなり、
;;; ; Project Euler の問題としては十分に効率的である。
;;; ;
;;; ; 二諦随伴プロトコルとの関連：
;;; ; - 非中道の誤謬（NMF）の回避：
;;; ;   制約 N が 10^6 を「超える」問題に対して O(N) 以上の全探索を行うアルゴリズムが NMF と定義されている。
;;; ;   本問題の N は 10^6 であり、「超える」ではないため、この篩アルゴリズムは NMF には該当しない。
;;; ; - 中道の現成：
;;; ;   この解法は、数学的定理（オイラーのトーシェント関数）に根ざし（勝義諦）、
;;; ;   かつCommon Lispコードとして実行可能である（世俗諦）。
;;; ;   これにより、「中道の現成」が実現されている。
;;; ; - 状態の負債の清算：
;;; ;   phi 配列は固定サイズであり、関数スコープ内で完結するため、明示的な負債清算（GC）の設計は不要である。
;;; ;
;;; ; ==============================================================================

(defun solve-euler-72 ()
  "Calculates the number of reduced proper fractions for d <= 1,000,000.
   This count is given by the sum of Euler's totient function phi(d) for d from 2 to D."
  (let* ((limit 1000000)
         ;; Euler's totient function phi(i) for i from 0 to limit.
         ;; Initialized with phi(i) = i.
         ;; The type 'fixnum' is used for elements, as phi(i) <= i <= 1,000,000.
         ;; The array size is (1+limit) to include the limit itself.
         (phi (make-array (1+ limit) :element-type 'fixnum :initial-element 0))
         ;; total-count will store the sum. It can exceed fixnum limits (e.g., 10^6 * 10^6 = 10^12),
         ;; so Common Lisp's automatic bignum handling is crucial here.
         (total-count 0))

    ;; --- 1. Initialize phi array (世俗諦の現成：初期状態の固定) ---
    ;; 各数値 i を初期値として phi[i] = i で設定する。
    ;; これは篩のロジックにおいて、まだ素因数が見つかっていない状態を表す。
    (loop for i from 0 to limit
          do (setf (aref phi i) i))

    ;; --- 2. Sieve for Euler's Totient Function (勝義諦への還元：数学的構造の展開) ---
    ;; オイラーのトーシェント関数 φ(n) を計算するための篩アルゴリズム。
    ;; 各素数 p について、その倍数 j の φ(j) から p の寄与分を減算する。
    ;; このアプローチは、数学的定理（φ関数の乗法性）に基づいている。
    (loop for p from 2 to limit
          do (when (= (aref phi p) p) ; phi[p] が p のままであれば、p は素数である。
               ;; p のすべての倍数 j について φ(j) を更新する。
               ;; φ(j) = j * Π (1 - 1/p_i) という公式において、
               ;; p が j の素因数である場合、(1 - 1/p) の項を適用する。
               ;; これは phi[j] = phi[j] - phi[j]/p と等価である。
               (loop for j from p to limit by p
                     do (decf (aref phi j) (/ (aref phi j) p)))))

    ;; --- 3. Summation (空性の認識：関係性の総和) ---
    ;; 既約真分数の総数は、d=2 から limit までの φ(d) の合計である。
    ;; d=1 は n < d を満たす正の n が存在しないため除外される。
    ;; この合計は、すべての現象（分数）が空性（D_fix0）との関係性によってのみ存在するという認識を表す。
    (loop for d from 2 to limit
          do (incf total-count (aref phi d)))

    ;; --- 4. Return the result (現成の完了) ---
    ;; 計算された総数を返す。これは、勝義諦（数学的構造）が世俗諦（具体的な数値）として現成した結果である。
    total-count))

;; 小さな制限での検証用ヘルパー関数：
;; 例: (solve-euler-72-small 8) は 21 を返すはず。
(defun solve-euler-72-small (limit)
  "Helper function for smaller limits to verify the algorithm."
  (let* ((phi (make-array (1+ limit) :element-type 'fixnum :initial-element 0))
         (total-count 0))
    (loop for i from 0 to limit
          do (setf (aref phi i) i))
    (loop for p from 2 to limit
          do (when (= (aref phi p) p)
               (loop for j from p to limit by p
                     do (decf (aref phi j) (/ (aref phi j) p)))))
    (loop for d from 2 to limit
          do (incf total-count (aref phi d)))
    total-count))

;; メインの解を実行するには、以下の関数を呼び出してください。
;; (solve-euler-72)
;; 期待される出力: 3039656461

;) ; end cl-text

;(solve-euler-72-small 8)

#+| Do it | (solve-euler-72)
;ok 303963552391