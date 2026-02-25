;;; ===========================================================================
;;; Euler 51: Prime Digit Replacements (Lisp Implementation)
;;; ===========================================================================

(cl:in-package "CL-USER")

(defparameter *limit* 1000000)
(defparameter *primes-sieve* (make-array *limit* :element-type 'bit :initial-element 1))

(defun prepare-sieve ()
  "エラトステネスのふるい：データベースの構築"
  (setf (bit *primes-sieve* 0) 0)
  (setf (bit *primes-sieve* 1) 0)
  (loop for i from 2 to (isqrt (1- *limit*))
        when (= 1 (bit *primes-sieve* i))
        do (loop for j from (* i i) below *limit* by i
                 do (setf (bit *primes-sieve* j) 0))))

(defun primep (n)
  (if (< n *limit*)
      (= 1 (bit *primes-sieve* n))
      ;; 範囲外は直接判定（今回の探索範囲ではほぼ不要）
      (loop for i from 2 to (isqrt n)
            never (zerop (mod n i)))))

(defun get-digit-positions (n digit)
  "数値 n の中にある指定した digit の位置をリストで返す（右から0番目）"
  (let ((pos '())
        (idx 0))
    (loop while (> n 0)
          do (multiple-value-bind (q r) (floor n 10)
               (when (= r digit) (push idx pos))
               (setf n q)
               (incf idx)))
    pos))

(defun replace-digits (n positions new-digit)
  "指定した位置の数字を new-digit に置き換える"
  (let ((digits (reverse (map 'list #'digit-char-p (format nil "~A" n)))))
    (dolist (p positions)
      (setf (nth p digits) new-digit))
    (parse-integer (map 'string #'digit-char (reverse digits)))))

(defun count-prime-family (p positions)
  "置き換えによって生成される素数ファミリーの数をカウントする"
  (let ((count 0)
        (first-member nil))
    (loop for d from 0 to 9
          do (let ((candidate (replace-digits p positions d)))
               ;; 先頭が0になる場合はカウントしない（桁数が変わるため）
               (unless (and (= d 0) (member (1- (length (write-to-string p))) positions))
                 (when (primep candidate)
                   (incf count)
                   (unless first-member (setf first-member candidate))))))
    (values count first-member)))

(defun solve-euler-51 (target-family-size)
  (prepare-sieve)
  (loop for p from 100 to *limit*
        when (primep p)
        do (let ((digits-str (write-to-string p)))
             ;; 各数字 (0, 1, 2) について置き換えを試みる
             ;; （8つのファミリーを作るには、3つの数字を置換しないと3の倍数判定で弾かれるため）
             (loop for d from 0 to 2
                   for positions = (get-digit-positions p d)
                   when (and positions (>= (length positions) 1))
                   do (let ((subsets (list positions))) ; 単純化のため全置換。必要なら部分集合を生成。
                        (dolist (pos-set subsets)
                          (multiple-value-bind (count first-prime) (count-prime-family p pos-set)
                            (when (>= count target-family-size)
                              (return-from solve-euler-51 first-prime)))))))))

;; 実行: (solve-euler-51 8)
;→ 121313
