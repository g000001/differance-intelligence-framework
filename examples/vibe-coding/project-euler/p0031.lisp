(defun solve-coin-sums ()
  (let* ((target 200)
         ;; 硬貨の種類（フロントラインの定義）
         (coins '(1 2 5 10 20 50 100 200))
         ;; DPテーブル（0pからtargetまでの「作り方の数」を保持する構造）
         (ways (make-array (1+ target) :initial-element 0)))
    
    ;; 初期状態：0pを作る方法は「何も使わない」という1通りのみ
    (setf (aref ways 0) 1)

    ;; 顕現列挙プロセス：各硬貨を順に導入し、構造を更新していく
    (dolist (coin coins)
      (loop for i from coin to target do
              (incf (aref ways i) (aref ways (- i coin)))))

    ;; ターゲットとなる200pの解を顕現させる
    (aref ways target)))

;; 実行と結果の表示
;;; (format t "Ways to make £2: ~d~%" (solve-coin-sums))
;;; ▻ Ways to make £2: 73682
;;; → nil
