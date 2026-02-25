;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "CL-USER")


(defun primep (n)
  "素数判定: 2, 3 を例外処理し、6k±1 の形で試し割りを行う"
  (cond ((<= n 1) nil)
        ((<= n 3) t)
        ((or (= 0 (mod n 2)) (= 0 (mod n 3))) nil)
        (t (let ((i 5))
             (loop while (<= (* i i) n)
                   do (if (or (= 0 (mod n i)) (= 0 (mod n (+ i 2))))
                          (return-from primep nil))
                   (incf i 6))
             t))))


(defun solve-euler-58 ()
  (let ((prime-count 0)
        (total-count 1)) ; 中心(1)を最初にカウント
    (loop for side-length from 3 by 2
          for step = (1- side-length)
          for corner = (* side-length side-length)
          do
          ;; 四隅のうち素数になりうる3点をチェック
          ;; 右下 (corner) は平方数なので常に素数ではない
          (when (primep (- corner step)) (incf prime-count))       ; 左下
          (when (primep (- corner (* 2 step))) (incf prime-count)) ; 左上
          (when (primep (- corner (* 3 step))) (incf prime-count)) ; 右上
          
          ;; 対角線上の数値の総数を更新 (+4)
          (incf total-count 4)
          
          ;; 比率が 10% を下回った時点で side-length を返して終了
          (when (< (/ prime-count total-count) 1/10)
            (return side-length)))))


#+| Do it | (format t "Result: ~A~%" (solve-euler-58))
;▻ Result: 26241
;→ nil


;;; *EOF*
