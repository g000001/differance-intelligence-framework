;;; この問題は、与えられた自然数 $k$ に対して、 $N = a_1 + a_2 + \cdots + a_k = a_1 \times a_2 \times \cdots \times a_k$ を満たす $N$ のうち最小のもの（最小積和数）を見つけ、それらの最小積和数のユニークな値の合計を計算することを求めます。$a_i$ は自然数（正の整数）であり、$k \ge 2$ です。

;;; **問題の分析とアプローチ:**

;;; 1.  **$N$ と $a_i$ の関係**:
;;;     $N = \sum_{i=1}^k a_i = \prod_{i=1}^k a_i$
;;;     $a_i \ge 1$ であるため、$a_i=1$ の項は積に影響せず、和に $1$ を加えます。
;;;     そこで、積に寄与する $a_i \ge 2$ の項の数を $k_g$ とし、それらの項を $b_1, \dots, b_{k_g}$ とします。残りの $k_1$ 個の項はすべて $1$ です。
;;;     すると、$k = k_g + k_1$ となります。
;;;     $N = \sum_{j=1}^{k_g} b_j + k_1$
;;;     $N = \prod_{j=1}^{k_g} b_j$

;;; 2.  **$k_1$ の導出**:
;;;     上記の2つの式から、$k_1 = N - \sum_{j=1}^{k_g} b_j$ を得ます。
;;;     $k_1 \ge 0$ である必要があるので、$N \ge \sum_{j=1}^{k_g} b_j$ が条件となります。
;;;     この条件は、$b_j \ge 2$ の場合、$k_g \ge 2$ で常に満たされます（$b_1 b_2 \cdots b_{k_g} \ge b_1 + b_2 + \cdots + b_{k_g}$）。$k_g=1$ の場合、$N=b_1$ となり、$k_1=0$ となるため、$k=1$ となり、$k \ge 2$ の条件に反します。したがって、$k_g \ge 2$ が有効な解に必須です。

;;; 3.  **$k$ の導出**:
;;;     $k = k_g + k_1 = k_g + (N - \sum_{j=1}^{k_g} b_j)$

;;; 4.  **探索範囲**:
;;;     *   $k$ の最大値は $12000$ です。
;;;     *   $N$ の最小値は $4$ です（$k=2$ の場合、$2+2=2 \times 2 = 4$）。
;;;     *   $N$ の最大値の見積もり: $k = k_g + (N - \sum b_j)$ です。$N$ は $k$ より大きくなります。$k_g=2$ で $b_1=2, b_2=M$ とすると、$N=2M$、$k=(2M - (2+M)) + 2 = M - 2 + 2 = M$。
;;;         つまり、$k=M$ のとき $N=2M$ という解が得られます。$k=12000$ の場合、$N=2 \times 12000 = 24000$ が候補となります。
;;;         したがって、$N$ の探索上限を $2 \times k_{max}$ より少し大きめに設定します（例: $25000$）。

;;; 5.  **アルゴリズム**:
;;;     *   `*min-product-sum-numbers*` という配列を `$k_{max}+1$` のサイズで作成し、すべての要素を `most-positive-fixnum` で初期化します。この配列は、インデックス $k$ に対応する最小積和数 $N$ を格納します。
;;;     *   $N$ を $2$ から推定された上限（例: $25000$）までループします。
;;;     *   各 $N$ について、そのすべての因数分解 $\{b_1, \dots, b_{k_g}\}$（ただし $b_j \ge 2$）を見つけます。これは再帰関数 `find-factorizations-recursive` で行います。
;;;         *   `find-factorizations-recursive(N, current-factors, remaining-N, start-divisor)`:
;;;             *   `remaining-N`: $N$ をこれまでの `current-factors` の積で割った残りの数。
;;;             *   `start-divisor`: 次の因数を探索する開始点（重複を避けるため）。
;;;             *   ベースケース: `remaining-N = 1` のとき、完全な因数分解が見つかったことを意味します。このとき、`current-factors` の要素を使って $k_g$, $\sum b_j$, $k_1$, $k$ を計算し、`*min-product-sum-numbers*[k]` を $N$ で更新します（より小さい値が見つかった場合のみ）。
;;;             *   再帰ステップ: `start-divisor` から `remaining-N` までの各整数 `d` に対して、`remaining-N` が `d` で割り切れる場合、`d` を `current-factors` に追加し、`remaining-N` を `remaining-N / d` に更新して再帰呼び出しを行います。
;;;     *   すべての $N$ の探索が完了した後、`*min-product-sum-numbers*` 配列を走査し、`most-positive-fixnum` 以外の値（つまり有効な最小積和数）をハッシュテーブルに格納して重複を排除します。
;;;     *   ハッシュテーブルに格納されたユニークな最小積和数の合計を計算して返します。

;;; **Common Lisp コード:**

;;; ```lisp
(defconstant +max-k+ 12000
  "考慮する k の最大値。問題文の $2 \le k \le 12000$ に対応。")

(defvar *min-product-sum-numbers* nil
  "各 k に対する最小積和数を格納する配列。
   インデックス k に、その k に対応する最小の N を格納する。")

(defun initialize-min-product-sum-numbers ()
  "min-product-sum-numbers 配列を非常に大きな値で初期化する。"
  (setq *min-product-sum-numbers*
        (make-array (1+ +max-k+) :initial-element most-positive-fixnum)))

(defun find-factorizations-recursive (N current-factors remaining-N start-divisor)
  "N を2以上の因数に分解し、対応する k を計算して最小積和数を更新する再帰関数。

   N: 現在調査している積和数候補。
   current-factors: これまでに選択された2以上の因数のリスト。
   remaining-N: N を current-factors の積で割った残りの値。
   start-divisor: 次に考慮する因数の最小値（重複する因数分解を避けるため）。"
  (declare (type fixnum N remaining-N start-divisor))
  (declare (optimize (speed 3) (safety 0) (debug 0)))

  ;; remaining-N が 1 になったら、完全な因数分解が見つかった。
  (when (= remaining-N 1)
    ;; current-factors は N の2以上の因数分解 {b_1, ..., b_k_g} を表す。
    (let* ((k-g (length current-factors))                  ; 2以上の因数の数
           (sum-of-factors (the fixnum (reduce #'+ current-factors :initial-value 0))) ; 2以上の因数の和
           (num-ones (the fixnum (- N sum-of-factors))))   ; 必要な 1 の数
      
      ;; 1 の数が負でないこと、および k が有効な範囲内にあることを確認。
      (when (>= num-ones 0)
        (let ((k (the fixnum (+ num-ones k-g))))           ; 全体の要素数 k
          (when (and (>= k 2) (<= k +max-k+))
            ;; k に対する最小積和数を更新。
            (setf (aref *min-product-sum-numbers* k)
                  (the fixnum (min (aref *min-product-sum-numbers* k) N)))))))
    (return-from find-factorizations-recursive nil))

  ;; 再帰ステップ: さらに因数を追加する。
  ;; d は start-divisor から remaining-N までを探索。
  ;; これにより、因数は非減少順に選ばれ、重複する因数分解の探索を避ける。
  (loop for d from start-divisor to remaining-N
        do (when (zerop (mod remaining-N d))
             (find-factorizations-recursive N
                                            (cons d current-factors)
                                            (floor remaining-N d)
                                            d))))

(defun solve-product-sum-problem ()
  "k が 2 から +max-k+ までのすべての最小積和数の合計を計算する。"
  (initialize-min-product-sum-numbers)

  ;; N の上限を推定。
  ;; k = 12000 の場合、N = 24000 (因数: 2 と 12000、残りは 1) が一つの解。
  ;; sum = 2 + 12000 + (24000 - (2+12000)) = 2 + 12000 + 11998 = 24000。
  ;; product = 2 * 12000 = 24000。
  ;; k = 2 (因数の数) + 11998 (1 の数) = 12000。
  ;; よって N は少なくとも 24000 まで探索する必要がある。安全のため少し余裕を持たせる。
  (let ((upper-bound-N (+ (* 2 +max-k+) 1000)))
    (loop for N from 2 to upper-bound-N
          do (find-factorizations-recursive N nil N 2))) ; 空の因数リスト、残りは N、最小因数は 2 から開始

  ;; 配列からユニークな最小積和数を集めて合計を計算する。
  (let ((unique-minimal-Ns (make-hash-table :test 'eql)))
    (loop for k from 2 to +max-k+
          do (let ((val (aref *min-product-sum-numbers* k)))
               ;; most-positive-fixnum でない場合、有効な最小積和数が見つかった。
               (unless (= val most-positive-fixnum)
                 (setf (gethash val unique-minimal-Ns) t)))) ; ハッシュテーブルに登録して重複を排除
    
    ;; ユニークな最小積和数の合計を計算して返す。
    (loop for N being the hash-key of unique-minimal-Ns
          sum N)))

;; 問題を解決し、結果を出力する。
;; (solve-product-sum-problem)
;;; ```


#+| Do it | (solve-product-sum-problem )
;→ 7587457
