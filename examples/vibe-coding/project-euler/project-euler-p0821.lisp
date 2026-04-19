#|;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0821 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0821)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(declaim (inline count-coprime-to-6))
(defun count-coprime-to-6 (m)
  "1からmまでの整数のうち、6と互いに素(2でも3でも割り切れない)な数の個数を包除原理で求める"
  (declare (type integer m))
  (+ (- m (floor m 2) (floor m 3)) (floor m 6)))

(defun solve ()
  (let* (($n 10000000000000000)
         ($omitted 0))
    (declare (type integer $n $omitted))
    
    (format t "Step 1: 空間のパリティ同値類への分解および O(log^2 N) の次元崩壊ループを開始...~%")
    
    ;; 2の奇数乗のループ
    (iterate (for a from 0)
             (declare (type fixnum a))
             (let ((pow2 (expt 2 (1+ (* 2 a)))))
               (declare (type integer pow2))
               (when (> pow2 $n) (leave))
               
               ;; 3の奇数乗のループ
               (iterate (for b from 0)
                        (declare (type fixnum b))
                        (let* ((pow3 (expt 3 (1+ (* 2 b))))
                               (p (* pow2 pow3)))
                          (declare (type integer pow3 p))
                          (when (> p $n) (leave))
                          
                          ;; p = 2^(2a+1) * 3^(2b+1) を固定し、条件を満たす y の数を集計
                          (let* ((m (floor $n p))
                                 (cnt (count-coprime-to-6 m)))
                            (declare (type integer m cnt))
                            (incf $omitted cnt)
                            
                            ;; 観測点での中間ログ（影響の大きい最初期の項のみ出力）
                            (when (and (= a 0) (= b 0))
                              (format t "  [Debug] a=0, b=0 (p=6): ~A elements excluded.~%" cnt)))))))
                              
    (format t "Step 2: 制約と周期性の適用完了。最悪計算回数は約230回。~%")
    
    (let (($ans (- $n $omitted)))
      (declare (type integer $ans))
      (format t "Final Result F(~A): ~A~%" $n $ans)
      $ans)))


9166666666666666 で不正解でした

過去の不正解の値は下記のとおりです。不正解の値を解析して正答を考察してください。
9249084249084257
9212454212454204
9214701193867859|#


;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash
(cl:in-package cl-user)
(defpackage #:project-euler-0821 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0821)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(declaim (inline count-coprime-to-6))
(defun count-coprime-to-6 (m)
  ;"1からmまでの整数のうち、6と互いに素(2でも3でも割り切れない)な数の個数を包除原理で求める"
  (declare (type (integer 0 *) m))
  (+ (- m (floor m 2) (floor m 3)) (floor m 6)))

(defun get-valid-masks (height)
  "連続する1を持たない高さHの有効なビットマスクを生成"
  (declare (type fixnum height))
  (let ((masks '()))
    (iterate (for m from 0 below (ash 1 height))
             (when (zerop (logand m (ash m 1)))
               (push m masks)))
    (nreverse masks)))

(defun solve ()
  (let* (($n 10000000000000000)
         ($v-list '())
         ($ans 0)
         ;; 状態遷移のメモ化テーブル (hash-tableで列の状態をキャッシュ)
         ($memo (make-hash-table :test 'equal)))
    (declare (type (integer 0 *) $n $ans))

    (format t "Step 1: グリッド形状を決定する 2^i * 3^j の閾値を生成中...~%")
    (iterate (for a from 0)
             (let ((pow2 (expt 2 a)))
               (when (> pow2 $n) (leave))
               (iterate (for b from 0)
                        (let* ((pow3 (expt 3 b))
                               (val (* pow2 pow3)))
                          (when (> val $n) (leave))
                          (push val $v-list)))))
    
    ;; 昇順にソートし、無限大の番兵を追加
    (setf $v-list (sort $v-list #'<))
    (let ((v-arr (make-array (length $v-list) :initial-contents $v-list))
          (k-max (length $v-list)))
      
      (format t "Step 2: ~A 個の独立した同値類 (グリッド形状) に対する Profile DP を実行...~%" k-max)
      
      (labels ((solve-dp (col M prev-mask)
                 "列col以降の最大カバー要素数を計算するメモ化DP"
                 (when (zerop M) (return-from solve-dp 0))
                 
                 (let* ((H (1+ (floor (log M 3))))
                        ;; 前の列のマスクのうち、現在の列に干渉する H+1 ビットのみを抽出
                        (trunc-prev (logand prev-mask (1- (ash 1 (1+ H)))))
                        (memo-key (list M trunc-prev))
                        (cached (gethash memo-key $memo)))
                   
                   (when cached (return-from solve-dp cached))
                   
                   (let ((max-reward 0)
                         (M-next (floor M 2))
                         (H-next (if (zerop (floor M 2)) 0 (1+ (floor (log (floor M 2) 3)))))
                         (forbidden (logior trunc-prev (ash trunc-prev -1))))
                     
                     (dolist (curr-mask (get-valid-masks H))
                       (declare (type fixnum curr-mask))
                       ;; 前の列との水平・アンチダイアゴナルの衝突判定
                       (when (zerop (logand curr-mask forbidden))
                         (let ((reward 0))
                           (declare (type fixnum reward))
                           ;; 各ビット(ノード)がカバーする要素数を計算
                           (iterate (for j from 0 below H)
                                    (when (logbitp j curr-mask)
                                      (incf reward) ; 自身
                                      (when (< j H-next) (incf reward)) ; Right
                                      (when (< (1+ j) H) (incf reward)))) ; Up
                           
                           (let ((total (+ reward (solve-dp (1+ col) M-next curr-mask))))
                             (when (> total max-reward)
                               (setf max-reward total))))))
                     
                     (setf (gethash memo-key $memo) max-reward)
                     max-reward))))

        (format t "Step 3: 各同値類に属する種 y の個数を掛け合わせて最終集計...~%")
        (iterate (for k from 0 below k-max)
                 (let* ((vk (aref v-arr k))
                        (vk-next (if (< (1+ k) k-max) (aref v-arr (1+ k)) (1+ $n)))
                        (y-max (floor $n vk))
                        (y-min (floor $n vk-next))
                        (count (- (count-coprime-to-6 y-max) (count-coprime-to-6 y-min))))
                   
                   (when (> count 0)
                     (let ((max-covered (solve-dp 0 vk 0)))
                       (incf $ans (* count max-covered)))))))
                       
      (format t "Final Result F(~A): ~A~%" $n $ans)
      $ans)))

