(defpackage "7c5b64b6-3892-5f5e-bba9-6a6a730cc525"
  (:use "CL"))

(in-package "7c5b64b6-3892-5f5e-bba9-6a6a730cc525")

(defun solve-euler-100 ()
  (let* ((target-total-discs 1000000000000) ; 10^12
         ;; Y > 2 * 10^12 - 1 となるYを探す
         (min-Y-threshold (- (* 2 target-total-discs) 1)))
    
    ;; 最初の有効な解 (B, T) = (3, 4) から開始する。
    ;; Y = 2T - 1 = 2*4 - 1 = 7
    ;; X = 2B - 1 = 2*3 - 1 = 5
    (let ((current-Y 7)
          (current-X 5))
      (loop
        ;; 次の解を計算
        (let ((next-Y (+ (* 3 current-Y) (* 4 current-X)))
              (next-X (+ (* 2 current-Y) (* 3 current-X))))
          (setf current-Y next-Y
                current-X next-X))
        
        ;; 現在のTが目標値を超えたかチェック
        ;; T = (current-Y + 1) / 2
        ;; B = (current-X + 1) / 2
        (when (> (/ (+ current-Y 1) 2) target-total-discs)
          ;; 条件を満たす最初のBを返す
          (return (/ (+ current-X 1) 2)))))))

;; 関数を実行して結果を得る
#+| Do it | (solve-euler-100 )
;; ok 756872327473
