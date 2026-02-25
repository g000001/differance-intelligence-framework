(defun pentagonal-p (n)
  "中道の不動点（逆関数）による五角数判定: n = x(3x-1)/2"
  (let ((x (/ (+ 1 (sqrt (+ 1 (* 24 n)))) 6)))
    (= x (floor x))))

(defun solve-pe44 ()
  "最小の差分 D (AC) を最善化探索"
  (loop for k from 2
        for pk = (/ (* k (- (* 3 k) 1)) 2)
        do (loop for j from (1- k) downto 1
                 for pj = (/ (* j (- (* 3 j) 1)) 2)
                 for diff = (- pk pj)
                 for sum = (+ pk pj)
                 when (and (pentagonal-p diff)
                           (pentagonal-p sum))
                 do (return-from solve-pe44 diff))))

;(format t "~A~%" (solve-pe44))
;▻ 5482660
;→ nil
