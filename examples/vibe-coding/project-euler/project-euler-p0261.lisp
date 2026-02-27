;; Title: Project Euler 261 (Square-pivots)
;; Engine: 二諦随伴 (F_n ⊣ G_{2^n+1}) プロトコル【Vieta Jumping 爆縮版】

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun solve-p261 (&optional (limit 10000000000))
  (declare (type (unsigned-byte 62) limit))
  (let ((visited (make-hash-table :test 'eql)))
    
    (labels ((check (A b c)
               (when (> A 1)
                 ;; F_n: 差延を削ぎ落とし、カノニカルな k と n の候補を抽出
                 (let ((num-k (+ (abs (- b c)) A -1))
                       (num-n (- (+ b c) A 1)))
                   (when (and (zerop (logand num-k 3))
                              (zerop (logand num-n 3)))
                     (let ((k (ash num-k -2))
                           (n (ash num-n -2)))
                       ;; 普遍性の条件 (m>0, n>=k, k<=limit) を満たす唯一の重心を固定
                       (when (and (> k 0) (<= k limit) (>= n k))
                         (setf (gethash k visited) t)))))))
             
             (dfs (x y z)
               ;; 対象の勝義諦（一意な重心）を現成
               (check x y z)
               (check y x z)
               (check z x y)
               
               ;; 左随伴 F_n による純化された枝の探索（Vietaの跳躍）
               (let ((nx (- (* 2 y z) x)))
                 ;; k_min のノルムが限界を超えた時点で Cauchy列を完全退化 (枝狩り)
                 (when (<= (ash (+ (- nx z) y -1) -2) limit)
                   (dfs y z nx)))
               
               (when (/= x y)
                 (let ((ny (- (* 2 x z) y)))
                   (when (<= (ash (+ (- ny z) x -1) -2) limit)
                     (dfs x z ny))))))
      
      ;; 右随伴 G_2n+1: 基底状態 (u, u, 2u^2-1) からの全域展開（共謀の完全破壊）
      (loop for u from 3 by 2
            for z = (1- (* 2 u u))
            for k-min = (ash (1- (* u u)) -1)
            while (<= k-min limit)
            do (dfs u u z)))
    
    ;; 差延を削ぎ落とした純粋なSquare-pivotsの総和
    (let ((sum 0))
      (declare (type (unsigned-byte 62) sum))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (incf sum k))
               visited)
      sum)))

;; 実行
(time (print (solve-p261 10000000000)))


238890850232021 
