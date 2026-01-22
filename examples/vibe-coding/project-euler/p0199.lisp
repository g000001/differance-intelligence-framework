(defun solve-pe199 (iterations)
  (let* ((k0 -1.0d0) ; 外円の曲率（内部を扱うため負値）
         (k1 (+ 1.0d0 (* 2.0d0 (/ (sqrt 3.0d0) 3.0d0)))) ; 内円の曲率
         (initial-area 1.0d0)
         (total-covered-area (* 3.0d0 (/ 1.0d0 (* k1 k1)))))

    (labels ((descartes (k1 k2 k3)
               "デカルトの円定理による新しい曲率の導出"
               (+ k1 k2 k3 (* 2.0d0 (sqrt (+ (* k1 k2) (* k2 k3) (* k3 k1))))))
             
             (fill-gap (k1 k2 k3 depth)
               "隙間を再帰的に充填（Ffix0への漸近）"
               (if (zerofow-p depth)
                   0.0d0
                   (let* ((kn (descartes k1 k2 k3))
                          (area (/ 1.0d0 (* kn kn))))
                     (+ area
                        (fill-gap kn k1 k2 (1- depth))
                        (fill-gap kn k2 k3 (1- depth))
                        (fill-gap kn k3 k1 (1- depth))))))
             
             (zerofow-p (d) (<= d 0)))

      ;; 1. 中央の隙間 (k1, k1, k1) × 1
      (setf total-covered-area 
            (+ total-covered-area (fill-gap k1 k1 k1 iterations)))
      
      ;; 2. 外円との隙間 (k0, k1, k1) × 3
      (setf total-covered-area 
            (+ total-covered-area (* 3.0d0 (fill-gap k0 k1 k1 iterations))))

      ;; 最終的な未カバー率（1 - 占有率）
      (format nil "~,8F" (- 1.0d0 total-covered-area)))))

(print (solve-pe199 10))
;▻ 
;▻ "0.00396087" 
;→ "0.00396087"

(defun solve-pe199-optimized (iterations)
  "16マトリクス分解を経て、再帰を空間操作に変換した顕現版"
  (let* ((k0 -1.0d0)
         (k1 (+ 1.0d0 (* 2.0d0 (/ (sqrt 3.0d0) 3.0d0))))
         ;; 3. 是の亦: 隙間の『3組』を最初から空間的にパッキングする
         (gaps (list (list k1 k1 k1) (list k0 k1 k1) (list k0 k1 k1) (list k0 k1 k1)))
         (total-area (* 3.0d0 (/ 1.0d0 (* k1 k1)))))

    (flet ((descartes (ks)
             (destructuring-bind (ka kb kc) ks
               (+ ka kb kc (* 2.0d0 (sqrt (+ (* ka kb) (* kb kc) (* kc ka))))))))

      ;; 9. 亦の是: APLの冪乗演算子のごとく、世代を一気に回す
      (dotimes (i iterations)
        (let ((next-gaps nil))
          (dolist (g gaps)
            (let* ((kn (descartes g))
                   (ka (first g)) (kb (second g)) (kc (third g)))
              ;; 新たな面積の現成 (1. 是の是)
              (incf total-area (/ 1.0d0 (* kn kn)))
              ;; 次世代の空間生成 (3. 是の亦)
              (push (list kn ka kb) next-gaps)
              (push (list kn kb kc) next-gaps)
              (push (list kn kc ka) next-gaps)))
          (setf gaps next-gaps))) ; 14. 非非の非: トポロジーの更新

      (format nil "~,8F" (- 1.0d0 total-area)))))

#+| Do it | (solve-pe199-optimized 10)
;→ "0.00396087"
