(defun solve ()
  "L <= 1,500,000 の範囲で、ちょうど1つの整数辺直角三角形を形成できるLの値を数えます。"
  (let* ((limit 1500000)
         ;; perimeters 配列は、周長 L を持つ異なるピタゴラスの三つ組の数を格納します。
         ;; 配列のインデックスは L、値はそのカウントです。
         (perimeters (make-array (1+ limit) :initial-element 0))
         ;; m の最大値を計算します。
         ;; 最小の原始周長は n=1 のときに 2*m*(m+1) です。
         ;; 2*m*(m+1) <= limit を満たす m の最大値を求めます。
         ;; m^2 + m - limit/2 <= 0
         ;; m = (-1 + sqrt(1 + 2*limit))/2
         (m-limit (floor (/ (- (sqrt (+ 1 (* 2 limit))) 1) 2)))
         (count-one-solution 0))

    ;; ユークリッドの公式 (m, n) に基づいて、すべての可能な m の値を反復処理します。
    (loop for m from 2 to m-limit
          do (loop for n from 1 below m
                   ;; 原始ピタゴラスの三つ組を生成するための m と n の条件:
                   ;; 1. m > n > 0 (ループの範囲で処理済み)
                   ;; 2. m と n は互いに素 (gcd(m, n) = 1)
                   ;; 3. m と n は異なる偶奇性を持つ (m+n が奇数)
                   when (and (= 1 (cl:gcd m n))
                             (= 1 (mod (+ m n) 2)))
                   do (let ((primitive-perimeter (* 2 m (+ m n))))
                        ;; 各原始周長について、limit までのその倍数を追加します。
                        ;; 各倍数 k * primitive-perimeter は、異なるピタゴラスの三つ組 (k*a, k*b, k*c) に対応します。
                        (loop for k from 1
                              for L = (* k primitive-perimeter)
                              while (<= L limit)
                              do (incf (aref perimeters L))))))

    ;; 'perimeters' 配列が完成した後、ちょうど1つの解を持つ周長 L の数を数えます。
    (loop for L from 1 to limit
          when (= 1 (aref perimeters L))
          do (incf count-one-solution))

    count-one-solution))

;; 解を実行するには、Common Lisp 環境で (solve) を呼び出します。
;; 例: (solve)
;; 結果は 164393 となります。 

#+| Do it | (solve )
;→ 161667
