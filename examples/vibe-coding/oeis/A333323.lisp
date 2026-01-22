;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "CL-USER")

;;; ============================================================
;;; Frontier-ACDP: Transfer Matrix Method for SAW
;;; 8x8 Grid "The Wall Breaker"
;;; ============================================================

(defparameter *n* 6)

(defun get-state (mask i)
  "i番目のマスの状態（0:空, 1:左端, 2:右端）を取得"
  (ldb (byte 2 (* i 2)) mask))

(defun set-state (mask i val)
  "i番目のマスの状態を更新"
  (dpb val (byte 2 (* i 2)) mask))

(defun find-match (mask pos dir)
  "括弧の対応関係（接続先）を探す"
  (let ((depth 0) (p pos))
    (loop
       (let ((st (get-state mask p)))
         (cond ((= st 1) (incf depth))
               ((= st 2) (decf depth)))
         (if (zerop depth) (return p))
         (incf p dir)))))

(defun solve-frontier ()
  (let ((dp (make-hash-table :test 'eql)))
    ;; 初期状態: 何もつながっていない状態が1通り
    (setf (gethash 0 dp) 1)
    
    (dotimes (r *n*)
      (dotimes (c *n*)
        (let ((new-dp (make-hash-table :test 'eql)))
          (maphash (lambda (mask count)
                     (let ((left (get-state mask c))
                           (up (get-state mask (1+ c))))
                       ;; 現在のマス (r, c) に入ってくる接続の状態に応じて分岐
                       (cond
                        ;; 1. どちらからも接続がない場合
                        ((and (= left 0) (= up 0))
                         ;; 新たな接続ペアを「右」と「下」に作る（1:左端, 2:右端）
                         (when (and (< (1+ c) (1+ *n*)) (< r (1- *n*)) (< c (1- *n*)))
                           (incf (gethash (set-state (set-state mask c 1) (1+ c) 2) new-dp 0) count))
                         ;; ここを通らない（空のまま）という選択（スタート/ゴール以外）
                         (unless (or (and (= r 0) (= c 0)) (and (= r (1- *n*)) (= c (1- *n*))))
                           (incf (gethash mask new-dp 0) count))
                         ;; スタート地点の特殊処理
                         (when (and (= r 0) (= c 0))
                           (incf (gethash (set-state mask c 1) new-dp 0) count)
                           (incf (gethash (set-state mask (1+ c) 1) new-dp 0) count)))

                        ;; 2. 片方からのみ接続がある場合（継続）
                        ((or (and (= left 0) (plusp up)) (and (plusp left) (= up 0)))
                         (let ((s (max left up)))
                           (incf (gethash (set-state (set-state mask c s) (1+ c) 0) new-dp 0) count)
                           (incf (gethash (set-state (set-state mask c 0) (1+ c) s) new-dp 0) count)))

                        ;; 3. 両方から接続がある場合（結合）
                        ((and (= left 1) (= up 1)) ; ( ( -> つなぎ変えて右側の ( を ) に
                         (incf (gethash (set-state
                                         (set-state
                                          (set-state mask (find-match mask (1+ c) 1) 1) c 0) (1+ c) 0)
                                        new-dp 0) count))
                        ((and (= left 2) (= up 2)) ; ) ) -> つなぎ変えて左側の ) を ( に
                         (incf (gethash (set-state
                                         (set-state
                                          (set-state mask (find-match mask c -1) 2) c 0) (1+ c) 0)
                                        new-dp 0) count))
                        ((and (= left 2) (= up 1)) ; ) ( -> 単純結合
                         (incf (gethash (set-state (set-state mask c 0) (1+ c) 0) new-dp 0) count))
                        ((and (= left 1) (= up 2)) ; ( ) -> 閉路形成
                         (when (and (= r (1- *n*)) (= c (1- *n*))) ; ゴールなら許可
                           (incf (gethash (set-state (set-state mask c 0) (1+ c) 0) new-dp 0) count))))))
                   dp)
          (setf dp new-dp)))
      ;; 行の終わりで状態をシフト（次元の正規化）
      (let ((next-row-dp (make-hash-table :test 'eql)))
        (maphash (lambda (mask count)
                   (if (= (get-state mask *n*) 0)
                       (incf (gethash (ash mask 2) next-row-dp 0) count)))
                 dp)
        (setf dp next-row-dp)))
    (gethash 0 dp)))

(defun run-frontier-8x8 ()
  (format t "--- Frontier-ACDP: Breaking 8x8 ---~%")
  (let ((start (get-internal-real-time)))
    (let ((result (solve-frontier)))
      (let ((end (get-internal-real-time)))
        (format t "顕現した経路の総数: ~D~%" result)
        (format t "計算時間: ~,3F 秒~%" (/ (- end start) internal-time-units-per-second))))))

;https://www.msi.co.jp/event/conference/uc2021/lp/pdf/msi2021_1_4.pdf
;(run-frontier-8x8)

;--- Frontier-ACDP: Breaking 8x8 ---
;顕現した経路の総数: 115156685746
;計算時間: 0.074 秒

;https://oeis.org/A333323
