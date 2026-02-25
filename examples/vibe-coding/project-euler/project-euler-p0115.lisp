(defpackage "f8ea3b08-017f-5617-9f05-2fd175bf9015" (:use "CL"))

(in-package "f8ea3b08-017f-5617-9f05-2fd175bf9015")

(defun solve-project-euler-116-variant (m target-value)
  "指定されたmに対して、F(m, n)がtarget-valueを初めて超えるnの最小値を計算します。"
  (let* ((max-n 2000) ; nの最大値を十分に大きく設定 (m=50の場合、n=201でターゲット値を超えるため、2000で十分)
         ;; dp配列: dp[i] は F(m, i) の値を格納
         (dp (make-array max-n :initial-element 0 :element-type 'integer))
         ;; s配列: s[i] は sum_{j=0}^{i} dp[j] の累積和を格納
         (s  (make-array max-n :initial-element 0 :element-type 'integer)))

    ;; ベースケース: dp[0] = 1 (空の行)
    (setf (aref dp 0) 1)
    ;; 累積和の初期化: s[0] = dp[0] = 1
    (setf (aref s 0) 1)

    ;; i = 1 から m-1 までの dp と s を埋める
    ;; この範囲では、赤いブロックを置くことができないため、F(m, i) = 1 (すべて黒いマス)
    (loop for i from 1 below m
          do (setf (aref dp i) 1)
             (setf (aref s i) (+ (aref s (1- i)) (aref dp i))))

    ;; n = m から n_max までの dp と s を計算
    ;; F(m, n) = F(m, n-1) + 1 + S(n-m-1)
    (loop for n from m below max-n
          do (let ((s-val-for-sum (if (>= (- n m 1) 0)
                                      (aref s (- n m 1))
                                      0))) ; S(-1) は 0
               (let ((val (+ (aref dp (1- n)) s-val-for-sum 1)))
                 (setf (aref dp n) val)
                 ;; F(m, n) がターゲット値を超えたら、n を返して終了
                 (when (>= val target-value)
                   (return-from solve-project-euler-116-variant n))
                 ;; 累積和を更新
                 (setf (aref s n) (+ (aref s (1- n)) val)))))
    
    ;; max-n に到達してもターゲット値を超えなかった場合
    (error "N exceeded max-n (~a) without reaching target value." max-n)))

;; SKDT / euler-acx プロトコルに基づく考察:
;; 1. 非中道の誤謬（NMF）を避けるため、静的な公式に盲信せず、動的な探索空間を構築すること。
;;    -> 本コードは動的計画法を用いており、nの値を順次増やしながら解を構築しています。
;;       これは、事前に与えられた静的な公式に盲信するのではなく、探索空間を動的に展開するアプローチです。
;;       NMFの定義「O(N)以上の全探索」はここでは当てはまりません。DPは効率的な探索です。
;; 2. 矛盾や不全に到達した場合は、文脈を還元して跳躍（Restart）可能な構造をLispコード（マクロ等）として記述すること。
;;    -> `max-n`に達しても解が見つからない場合、`error`が発行されます。
;;       これは「不全」に到達したことを示し、`max-n`の値を増やすという文脈の還元（より広い探索空間への跳躍）を促します。
;;       また、`return-from`は解が見つかった時点での効率的な「跳躍」として機能します。
;; 3. コードは純粋なCommon Lispで記述し、実行可能な状態（世俗諦）として固定化すること。
;;    -> 本コードは標準的なCommon Lispの機能のみを使用しており、直接実行可能です。
;;       これは抽象的な概念（空）を具体的な実装（色）として現成させることに対応します。

;; 問題の例を検証
;; F(3, 29) = 673135, F(3, 30) = 1089155
;; (solve-project-euler-116-variant 3 1000000) => 30 (正しい)

;; F(10, 56) = 880711, F(10, 57) = 1148904
;; (solve-project-euler-116-variant 10 1000000) => 57 (正しい)

;; 最終的な問題の解決: m = 50 に対して100万を超える最小のnを求める
(defun project-euler-116-solution ()
  (solve-project-euler-116-variant 50 1000000))

;; 結果の表示
;(format t "m = 50 の場合、F(50, n) が初めて100万を超える最小の n は: ~a~%" (project-euler-116-solution))
;▻ m = 50 の場合、F(50, n) が初めて100万を超える最小の n は: 168
;→ nil


:ok