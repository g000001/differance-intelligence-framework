;;; Common Lispでこのパズルを解くためのコードを以下に示します。

;;; このコードは、以下の手順で問題を解決します。
;;; 1.  **順列の生成**: 4つの数字のすべての順列を生成します。
;;; 2.  **式の生成と評価**:
;;;     *   再帰的な関数 `generate-all-expressions` を使用して、与えられた数字のリストから、すべての可能な括弧の配置と4つの算術演算子（`+`, `-`, `*`, `/`）の組み合わせを生成します。
;;;     *   中間結果が分数になる可能性があるため、Common Lispの`RATIO`型を自動的に利用する算術関数を使用します。
;;;     *   ゼロ除算は`NIL`を返して無効な計算として扱います。
;;; 3.  **正の整数ターゲットの収集**: 生成されたすべての結果から、正の整数のみを抽出し、重複を排除してソートします。
;;; 4.  **連続する整数の最長シーケンスの特定**: ソートされた正の整数リストから、1から始まる連続した整数の最長シーケンスの長さを計算します。
;;; 5.  **すべての数字の組み合わせを試行**: 1から9までの数字から4つの異なる数字を選ぶすべての組み合わせ（`a < b < c < d`）をループし、上記の手順を適用して、最長の連続シーケンスを生成する数字のセットを見つけます。

;;; ```lisp
;;; 1. 順列の生成
(defun permutations (list)
  "リストのすべての順列を生成します。"
  (if (null list)
      (list nil)
      (loop for x in list
            append (loop for p in (permutations (remove x list :count 1))
                         collect (cons x p)))))

;;; 2. 算術演算の実行（Rational数対応）
(defun calculate-rational (r1 r2 op)
  "2つのRational数に対して算術演算を実行します。
   ゼロ除算の場合は NIL を返します。"
  (handler-case
      (case op
        (+ (+ r1 r2))
        (- (- r1 r2))
        (* (* r1 r2))
        (/ (if (zerop r2) nil (/ r1 r2))) ; ゼロ除算チェック
        (t nil))
    (error () nil))) ; その他の算術エラーを捕捉

;;; 3. すべての可能な式を生成し、評価する
(defun generate-all-expressions (numbers)
  "数字のリストから、すべての可能な括弧の配置と演算子を使用して、
   すべての可能なRational数を生成します。"
  (if (= (length numbers) 1)
      (list (car numbers)) ; 基底ケース：数字が1つだけなら、その数字を返す
      (let ((results '()))
        ;; 数字のリストを2つの非空のサブセットに分割するすべての方法を反復処理
        (loop for i from 1 below (expt 2 (length numbers))
              do (let* ((subset1-indices (loop for bit from 0 below (length numbers)
                                                when (logbitp bit i) collect bit))
                        (subset2-indices (loop for bit from 0 below (length numbers)
                                                unless (logbitp bit i) collect bit)))
                   ;; 両方のサブセットが非空であることを確認
                   (when (and subset1-indices subset2-indices)
                     (let* ((subset1 (mapcar #'(lambda (idx) (nth idx numbers)) subset1-indices))
                            (subset2 (mapcar #'(lambda (idx) (nth idx numbers)) subset2-indices))
                            (results1 (generate-all-expressions subset1)) ; 再帰的にサブセット1の式を生成
                            (results2 (generate-all-expressions subset2))) ; 再帰的にサブセット2の式を生成
                       ;; サブセット1とサブセット2の結果をすべての演算子で結合
                       (loop for r1 in results1
                             do (loop for r2 in results2
                                      do (dolist (op '(+ - * /))
                                           (let ((val (calculate-rational r1 r2 op)))
                                             (when val
                                               (push val results))))))))))
        (remove-duplicates results :test #'=)))) ; 重複する結果を排除

;;; 4. 正の整数ターゲットを抽出
(defun get-positive-integer-targets (rational-results)
  "Rational数のリストから、重複のない正の整数をソートして返します。"
  (sort (remove-duplicates
         (loop for r in rational-results
               when (and (rationalp r) ; Rational数であることを確認 (整数も含む)
                         (>= r 1)      ; 正であることを確認
                         (integerp r)) ; 整数であることを確認
               collect r)
         :test #'=)
        #'<)) ; 昇順にソート

;;; 5. 1から始まる連続する整数の最長シーケンスの長さを検索
(defun find-longest-consecutive-sequence (target-numbers)
  "ソートされた重複のない正の整数のリストが与えられた場合、
   1から始まる連続する整数の最長シーケンスの長さを検索します。"
  (loop for i from 1
        for num in target-numbers
        while (= i num)
        finally (return (1- i))))

;;; メインの解決関数
(defun solve-puzzle ()
  "1から9までの4つの異なる数字 (a < b < c < d) のセットを見つけ、
   1から始まる連続する正の整数の最長シーケンスを生成するものを返します。"
  (let ((max-n 0)
        (best-digits-str "")
        (all-digit-sets '()))
    
    ;; 1から9までの4つの数字のすべての組み合わせを生成 (a < b < c < d の順)
    (loop for a from 1 to 6
          do (loop for b from (1+ a) to 7
                   do (loop for c from (1+ b) to 8
                            do (loop for d from (1+ c) to 9
                                       :do (push (list a b c d) all-digit-sets)))))
    
    (setf all-digit-sets (nreverse all-digit-sets)) ; ログ出力のため昇順に処理
    
    (format t "パズルソルバーを開始します...~%")
    (loop for digits in all-digit-sets
          do (let* ((current-digits-str (format nil "~{~a~}" digits))
                    ;; 結果の重複を効率的に収集するためにハッシュテーブルを使用
                    (unique-rational-results-ht (make-hash-table :test #'equal))) 
               
               ;; 現在の数字セットのすべての順列を生成
               (dolist (perm (permutations digits))
                 ;; 各順列から式を生成
                 (dolist (result (generate-all-expressions perm))
                   (setf (gethash result unique-rational-results-ht) t)))
               
               (let* ((all-results-list (loop for k being the hash-key of unique-rational-results-ht collect k))
                      (target-numbers (get-positive-integer-targets all-results-list))
                      (n (find-longest-consecutive-sequence target-numbers)))
                 
                 ;; 最長シーケンスが見つかった場合は更新
                 (when (> n max-n)
                   (setf max-n n)
                   (setf best-digits-str current-digits-str))
                 (format t "数字 ~a -> 最長連続N: ~a (現在の最大: ~a, 最良セット: ~a)~%"
                         current-digits-str n max-n best-digits-str))))
    
    (format t "~%最終回答: 最良の数字: ~a (最長連続N: ~a)~%" best-digits-str max-n)
    best-digits-str))

;;; 実行例
;;; (solve-puzzle)
;;; ```


#+| Do it | (solve-puzzle )