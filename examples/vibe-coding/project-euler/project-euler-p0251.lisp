;; Title: Project Euler 251 (Cardano Triplets)
;; Engine: 二諦随伴 (F_n ⊣ G_{2^n+1}) プロトコル【GC完全ゼロ・レジスタ爆縮版】

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; GCを一切発生させない完全fixnum化されたGCD
(declaim (inline fast-gcd))
(defun fast-gcd (a b)
  (declare (type fixnum a b))
  (loop while (> b 0) do
    (let ((temp (mod a b)))
      (declare (type fixnum temp))
      (setf a b b temp)))
  a)

;;; GCを一切発生させない完全fixnum化された拡張ユークリッド
(declaim (inline fast-mod-inverse))
(defun fast-mod-inverse (a m)
  (declare (type fixnum a m))
  (let ((s 0) (old-s 1)
        (r m) (old-r a))
    (declare (type fixnum s old-s r old-r))
    (loop while (> r 0) do
      (let ((q (floor old-r r)))
        (declare (type fixnum q))
        (let ((temp-r (the fixnum (- old-r (the fixnum (* q r))))))
          (setf old-r r r temp-r))
        (let ((temp-s (the fixnum (- old-s (the fixnum (* q s))))))
          (setf old-s s s temp-s))))
    (if (< old-s 0)
        (the fixnum (+ old-s m))
        old-s)))

(defun solve-p251 (&optional (n 110000000))
  (declare (type fixnum n))
  (let ((count 0)
        (e-max (isqrt (truncate (the fixnum (* 8 n)) 3))))
    (declare (type fixnum count e-max))
    
    (loop for e of-type fixnum from 1 to e-max by 2 do
      (let ((e2 (the fixnum (* e e))))
        (declare (type fixnum e2))
        (loop for f of-type fixnum from 1 do
          ;; コンパイラに「これは絶対に62bitに収まる」と強制(the fixnum)し、Bignum割当を破壊
          (let ((num (the fixnum (+ (the fixnum (* (the fixnum (1+ n)) e2))
                                    (the fixnum (* 3 (the fixnum (* f f)))))))
                (den (the fixnum (+ (the fixnum (* 3 (the fixnum (* f e2))))
                                    (the fixnum (* e e2))
                                    (the fixnum (* 8 (the fixnum (* f (the fixnum (* f f))))))))))
            (declare (type fixnum num den))
            
            ;; 停止条件
            (if (< num den) 
                (return))
            
            ;; GCゼロの高速GCDと逆元計算
            (when (= (fast-gcd e f) 1)
              (let* ((inv8f (fast-mod-inverse (mod (the fixnum (* 8 f)) e2) e2))
                     (d0 (mod (the fixnum (* 3 inv8f)) e2)))
                (declare (type fixnum inv8f d0))
                (when (zerop d0) (setf d0 e2))
                
                (let ((d-max (floor num den)))
                  (declare (type fixnum d-max))
                  (when (>= d-max d0)
                    (incf count (the fixnum (1+ (floor (the fixnum (- d-max d0)) e2))))))))))))
    count))

;; 実行
(time (print (solve-p251 110000000)))
