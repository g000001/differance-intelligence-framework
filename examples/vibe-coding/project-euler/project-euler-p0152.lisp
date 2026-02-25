
;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0152 (:use cl alexandria))
;;; (in-package #:project-euler-0152)

;;; ;; ==============================================================================
;;; ;; 1. 二諦随伴（Two-Truths Entanglement）に基づく設計
;;; ;; ------------------------------------------------------------------------------
;;; ;; 世俗諦（Conventional Truth）：
;;; ;;   1/n^2 の和を 1/2 に近づける探索問題。
;;; ;; 勝義諦（Ultimate Truth）：
;;; ;;   p進付値（p-adic valuation）による分母の消去可能性条件。
;;; ;;   特定の素数 p について、分母から p^2 を消去できない数は、空（Sunyata）
;;; ;;   として探索空間から排除される。
;;; ;; ==============================================================================

;;; (defun prime-factors (n)
;;;   "nの素因数をリストで返す（世俗的分解）。"
;;;   (let ((factors '())
;;;         (temp n)
;;;         (d 2))
;;;     (loop while (<= (* d d) temp)
;;;           do (if (zerop (mod temp d))
;;;                  (progn (push d factors)
;;;                         (loop while (zerop (mod temp d))
;;;                               do (setf temp (/ temp d))))
;;;                  (incf d)))
;;;     (when (> temp 1) (push temp factors))
;;;     factors))

;;; (defun v-p (n p)
;;;   "nにおける素数pの指数（p進付値 v_p(n)）。"
;;;   (if (zerop n) 0
;;;       (let ((count 0)
;;;             (temp n))
;;;         (loop while (zerop (mod temp p))
;;;               do (incf count) (setf temp (/ temp p)))
;;;         count)))

;;; (defun solve-euler-152 (&optional (limit 80))
;;;   "Project Euler 152 を解く。二諦スライス圏における探索を行う。"
;;;   (let* ((target 1/2)
;;;          ;; 候補の絞り込み（勝義諦による制約）：
;;;          ;; 素数 p >= 17 の倍数は、範囲内で p^2 を消去できないため排除。
;;;          ;; また、p^2 (p >= 5) の倍数も、同様の理由で排除。
;;;          (candidates (loop for n from 2 to limit
;;;                            for factors = (prime-factors n)
;;;                            when (and (every (lambda (p) (<= p 13)) factors)
;;;                                      (<= (v-p n 3) 2) ; 3^3=27 以上は排除
;;;                                      (<= (v-p n 5) 1) ; 5^2=25 以上は排除
;;;                                      (<= (v-p n 7) 1)) ; 7^2=49 以上は排除
;;;                            collect n))
;;;          ;; 探索の効率化のため降順にソート（跳躍の準備）
;;;          (sorted (coerce (sort candidates #'>) 'vector))
;;;          (len (length sorted))
;;;          (inv-sq-cands (map 'vector (lambda (n) (/ 1 (* n n))) sorted))
;;;          ;; 枝刈り用の累積和
;;;          (max-rem (make-array (1+ len) :initial-element 0))
;;;          ;; 各素数が出現する最後のインデックス（中道の現成地点）
;;;          (last-prime-at (make-array len :initial-element nil)))

;;;     ;; 累積和の計算
;;;     (loop for i from (1- len) downto 0
;;;           do (setf (aref max-rem i) (+ (aref max-rem (1+ i)) (aref inv-sq-cands i))))

;;;     ;; 各素数 p に対する最後のインデックスを特定
;;;     (dolist (p '(3 5 7 11 13))
;;;       (let ((pos (position-if (lambda (n) (zerop (mod n p))) sorted :from-end t)))
;;;         (when pos (setf (aref last-prime-at pos) p))))

;;;     ;; 探索エンジン：二諦の境界を渡る
;;;     (labels ((count-ways (idx current-sum)
;;;                (cond
;;;                  ;; 目標到達：中道の現成
;;;                  ((= current-sum target) 1)
;;;                  
;;;                  ;; 枝刈り：世俗的限界（これ以上足しても届かない、または超えている）
;;;                  ((or (= idx len) (> current-sum target)) 0)
;;;                  ((< (+ current-sum (aref max-rem idx)) target) 0)
;;;                  
;;;                  (t
;;;                   (let ((ways 0))
;;;                     ;; 選択肢1：n を採用する（色：Shiki）
;;;                     (let ((next-sum (+ current-sum (aref inv-sq-cands idx)))
;;;                           (p (aref last-prime-at idx)))
;;;                       ;; p進制約のチェック：その素数の最後の出現箇所で分母に残っていれば、
;;;                       ;; ターゲット(1/2)に到達できない（勝義諦による否定）。
;;;                       (unless (and p (zerop (mod (denominator next-sum) p)))
;;;                         (incf ways (count-ways (1+ idx) next-sum))))
;;;                     
;;;                     ;; 選択肢2：n を採用しない（空：Ku）
;;;                     (let ((p (aref last-prime-at idx)))
;;;                       (unless (and p (zerop (mod (denominator current-sum) p)))
;;;                         (incf ways (count-ways (1+ idx) current-sum))))
;;;                     
;;;                     ways)))))
;;;       (count-ways 0 0))))

;;; ;; 実行
;;; ;(format t "Result for limit 80: ~A~%" (solve-euler-152 80))


;;; #+| Do it | (solve-euler-152 )


;;; -*- mode: Lisp; coding: utf-8 -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0152
  (:use #:cl #:alexandria)
  (:export #:solve))
(in-package #:project-euler-0152)

;; -----------------------------------------------------------------------------
;; 1. 二諦随伴：素数グループによる「空」の事前計算
;; -----------------------------------------------------------------------------

(defun get-valid-sums (p candidates target-denom)
  "素数 p の倍数の集合から、和の分母に p が残らない組み合わせを列挙する。"
  (let ((results (list 0)))
    (dolist (c candidates)
      (let ((val (/ 1 (* c c)))
            (new-results '()))
        (dolist (current results)
          (let ((next (+ current val)))
            ;; 勝義的制約：和の分母の p 進付値が target-denom 以下であれば許容
            (push next new-results)))
        (setf results (append results new-results))))
    ;; 分母から p が消えている（または十分に消去されている）ものだけを抽出
    (remove-if-not (lambda (x) 
                     (if (zerop x) t
                         (let ((den (denominator x)))
                           (not (zerop (mod den p))))))
                   (remove-duplicates results))))

(defun solve-euler-152 (&optional (limit 80))
  "素数ごとに探索空間を爆縮させ、中道を現成する。"
  (let* ((primes '(13 11 7 5 3 2))
         ;; 各素数 p について、limit 内の倍数を抽出
         (p-groups (loop for p in primes
                         collect (loop for n from 2 to limit
                                       when (zerop (mod n p))
                                       collect n)))
         ;; 13, 11, 7 ... と順番に「分母からその素数が消える和」を確定させていく
         (current-valid-sums '(0)))

    (format t "Calculating valid sub-sums for each prime group...~%")

    ;; 素数 13, 11, 7 については、単独で「分母から消える」必要がある。
    ;; なぜなら、他の素数の倍数と組み合わせてもこれらの素数は消せないから（勝義的必然）。
    (dolist (p '(13 11 7))
      (let* ((multiples (loop for n from 2 to limit
                              ;; まだ使っていない p の倍数（かつ、より大きい素数を含まない）
                              when (and (zerop (mod n p))
                                        (every (lambda (px) (or (<= px p) (not (zerop (mod n px)))))
                                               '(13 11 7)))
                              collect n))
             (valid-parts (get-valid-sums p multiples p)))
        (format t "  Prime ~2A: found ~A valid sub-sums.~%" p (length valid-parts))
        (let ((new-total-sums '()))
          (dolist (v valid-parts)
            (dolist (s current-valid-sums)
              (push (+ s v) new-total-sums)))
          (setf current-valid-sums (remove-duplicates new-total-sums)))))

    ;; 残りの 2, 3, 5 の倍数およびその他の数は、数が多いので通常の DFS に累積和の枝刈りを組み合わせる
    (let* ((used-numbers (loop for p in '(13 11 7 5)
                               append (loop for n from 2 to limit when (zerop (mod n p)) collect n)))
           (remaining-numbers (loop for n from 2 to limit
                                    unless (member n used-numbers)
                                    collect n))
           ;; 探索効率のため降順ソート
           (sorted-rem (sort (remove-duplicates remaining-numbers) #'>))
           (inv-sq (map 'vector (lambda (n) (/ 1 (* n n))) sorted-rem))
           (len (length inv-sq))
           (max-rem (make-array (1+ len) :initial-element 0))
           (total-count 0))
      
      (loop for i from (1- len) downto 0
            do (setf (aref max-rem i) (+ (aref max-rem (1+ i)) (aref inv-sq i))))

      (format t "Starting final search with ~A initial sums and ~A remaining numbers...~%" 
              (length current-valid-sums) len)

      (labels ((dfs (idx current-sum)
                 (cond
                   ((= current-sum 1/2) (incf total-count))
                   ((or (= idx len) (> current-sum 1/2)) nil)
                   ((< (+ current-sum (aref max-rem idx)) 1/2) nil)
                   (t
                    ;; 3 や 5 の p 進制約（最後の出現場所で分母をチェック）をここに加えるとさらに加速
                    (dfs (1+ idx) (+ current-sum (aref inv-sq idx)))
                    (dfs (1+ idx) current-sum)))))

        (dolist (initial-sum current-valid-sums)
          (dfs 0 initial-sum))
        total-count))))


#+| Do it | (solve-euler-152 )


(defun solve-euler-152-final ()
  (let* ((all-candidates ; 13, 11, 7, 5, 3, 2 の倍数から選別された精鋭たち
          (remove-if-not (lambda (n) 
                           (let ((fs (prime-factors n)))
                             (every (lambda (p) (member p '(2 3 5 7 11 13))) fs)))
                         (loop for i from 2 to 80 collect i)))
         (len (length all-candidates))
         (mid (floor len 2))
         (side-a (subseq all-candidates 0 mid))
         (side-b (subseq all-candidates mid))
         (sums-a (make-hash-table :test 'eql)))

    (format t "Candidates: ~A (Split into ~A and ~A)~%" len (length side-a) (length side-b))

    ;; 1. グループAの全ての部分和をハッシュに格納 (Meet-in-the-middleの前半)
    (labels ((gen-a (idx current-sum)
               (if (= idx (length side-a))
                   (setf (gethash current-sum sums-a) (1+ (gethash current-sum sums-a 0)))
                   (progn
                     (gen-a (1+ idx) (+ current-sum (/ 1 (* (nth idx side-a) (nth idx side-a)))))
                     (gen-a (1+ idx) current-sum)))))
      (gen-a 0 0))

    (format t "Side A generated ~A unique sums. Starting Search B...~%" (hash-table-count sums-a))

    ;; 2. グループBを探索し、(1/2 - sum-b) がハッシュにあるか確認
    (let ((total-ways 0))
      (labels ((gen-b (idx current-sum)
                 (if (= idx (length side-b))
                     (let ((needed (- 1/2 current-sum)))
                       (incf total-ways (gethash needed sums-a 0)))
                     (progn
                       (gen-b (1+ idx) (+ current-sum (/ 1 (* (nth idx side-b) (nth idx side-b)))))
                       (gen-b (1+ idx) current-sum)))))
        (gen-b 0 0))
      total-ways)))


#+| Do it | (solve-euler-152-final )
;301
:fix :gemini-pro-3.1