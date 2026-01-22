;;; ===========================================================================
;;; Euler 54: Poker Hands - Pure Rule Simulation
;;; ===========================================================================

(defun card-value (char)
  (let ((val (position char "23456789TJQKA")))
    (if val (+ val 2) 0)))

(defun parse-hand (card-strings)
  (loop for s in card-strings
        collect (cons (card-value (char s 0)) (char s 1))))

(defun evaluate-hand (hand)
  "手札を評価し、常に長さ6の数値リストを返すことで nil 比較を回避する"
  (let* ((values (sort (mapcar #'car hand) #'>))
         (suits (mapcar #'cdr hand))
         ;; (値 . 個数) のハッシュを作成
         (counts-map (let ((h (make-hash-table)))
                       (dolist (v values) (incf (gethash v h 0)))
                       h))
         ;; 個数優先、値優先でソートした一意な値リスト
         (sorted-by-count (sort (remove-duplicates values)
                                (lambda (a b)
                                  (let ((ca (gethash a counts-map))
                                        (cb (gethash b counts-map)))
                                    (if (/= ca cb) (> ca cb) (> a b))))))
         (counts (sort (loop for v being the hash-values of counts-map collect v) #'>))
         (flush (every (lambda (s) (char= s (cdr (first hand)))) suits))
         (straight (and (= (length (remove-duplicates values)) 5)
                        (= (- (first values) (fifth values)) 4))))
    
    ;; 全ての役の結果を「(役ランク キッカー1 キッカー2 キッカー3 キッカー4 キッカー5)」
    ;; という固定長（長さ6）のリストにパディング（0埋め）して返す。
    (let ((result
           (cond
             ((and straight flush (= (first values) 14)) (list 10)) ; Royal Flush
             ((and straight flush) (list 9 (first values)))         ; Straight Flush
             ((equal counts '(4 1)) (list 8 (first sorted-by-count) (second sorted-by-count)))
             ((equal counts '(3 2)) (list 7 (first sorted-by-count) (second sorted-by-count)))
             (flush (cons 6 values))                                ; Flush
             (straight (list 5 (first values)))                     ; Straight
             ((equal counts '(3 1 1)) (cons 4 sorted-by-count))     ; Three of a Kind
             ((equal counts '(2 2 1)) (cons 3 sorted-by-count))     ; Two Pairs
             ((equal counts '(2 1 1 1)) (cons 2 sorted-by-count))   ; One Pair
             (t (cons 1 values)))))                                 ; High Card
      ;; 6要素に満たない場合は 0 で埋める（比較エラー回避の縫合）
      (append result (make-list (- 6 (length result)) :initial-element 0)))))

(defun player-1-wins-p (line)
  "辞書式比較による勝敗判定"
  (let* ((all-cards (uiop:split-string line))
         (p1-eval (evaluate-hand (parse-hand (subseq all-cards 0 5))))
         (p2-eval (evaluate-hand (parse-hand (subseq all-cards 5 10)))))
    ;; 長さが同じ固定長リスト同士を比較するため、nil エラーは発生しない
    (loop for v1 in p1-eval
          for v2 in p2-eval
          do (cond ((> v1 v2) (return t))
                   ((< v1 v2) (return nil))))))

(defun solve-euler-54 (file-path)
  (with-open-file (in file-path)
    (loop for line = (read-line in nil)
          while line
          count (player-1-wins-p line))))


#+| Do it | (solve-euler-54 "poker.txt")
