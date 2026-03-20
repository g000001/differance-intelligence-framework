;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0470 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0470)

(declaim (inline binom calc-n-k))

(defun binom (n k)
  "二項係数をdouble-floatで計算する。最大でC(19, 9)なので浮動小数点誤差は発生しない。"
  (declare (type fixnum n k))
  (if (or (< k 0) (> k n))
      0.0d0
      (let ((res 1.0d0))
        (declare (type double-float res))
        (iterate (for i from 1 to k)
          (setf res (/ (* res (float (+ (- n i) 1) 0.0d0)) (float i 0.0d0))))
        res)))

(defun calc-n-k (d k)
  "重みkの状態への期待訪問回数を閉じた式から計算する。"
  (declare (type fixnum d k))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (iterate (for j from 0 below k)
      (incf sum (/ 1.0d0 (binom (- d 1) j))))
    sum))

(defun solve (&optional (n 20))
  (declare (type fixnum n))
  (format t "Calculating F(~A)...~%" n)
  
  ;; 不変量3：和の順序交換のための係数テーブル C(|V|, max(V)) を事前計算
  (let ((c-table (make-array '(25 25) :element-type 'double-float :initial-element 0.0d0)))
    (declare (type (simple-array double-float (25 25)) c-table))
    (iterate (for k from 1 to n)
      ;; 修正点：部分集合の最大要素 m-val は、必ず要素数 k 以上であるため k から n までループ
      (iterate (for m-val from k to n)
        (let ((sum 0.0d0))
          (declare (type double-float sum))
          (iterate (for d from (max 4 m-val) to n)
            (incf sum (calc-n-k d k)))
          (setf (aref c-table k m-val) sum))))
    
    (let ((total-expected 0.0d0)
          (arr (make-array 25 :element-type 'double-float :initial-element 0.0d0)))
      (declare (type double-float total-expected)
               (type (simple-array double-float (25)) arr))
      
      (let ((limit (ash 1 n)))
        (declare (type fixnum limit))
        ;; すべての部分集合 V をビットマスクで走査
        (iterate (for v-mask from 1 below limit)
          (let* ((k (logcount v-mask))
                 (m-val (integer-length v-mask))
                 (w (aref c-table k m-val)))
            (declare (type fixnum k m-val)
                     (type double-float w))
            
            (when (> w 0.0d0)
              (let ((idx 0)
                    (inv-k (/ 1.0d0 k)))
                (declare (type fixnum idx)
                         (type double-float inv-k))
                
                ;; Vの要素を抽出し昇順に配列へ格納
                (iterate (for i from 0 below n)
                  (when (logbitp i v-mask)
                    (setf (aref arr idx) (float (1+ i) 0.0d0))
                    (incf idx)))
                
                ;; c = 0 のときの最適期待値は Vの最大値 (arrの最後の要素)
                (let ((sum-p (aref arr (1- k)))
                      (e1 0.0d0))
                  (declare (type double-float sum-p e1))
                  
                  ;; ターン1での期待値 E_1 の計算
                  (iterate (for i from 0 below k)
                    (incf e1 (aref arr i)))
                  (setf e1 (* e1 inv-k))
                  
                  ;; 各コスト c について最適停止の期待値を計算
                  (iterate (for c from 1 to n)
                    (let ((c-float (float c 0.0d0)))
                      (declare (type double-float c-float))
                      
                      ;; 最初のターンの増分すらコストを下回る場合は即座に打ち切り (P_opt = 0)
                      (when (< e1 c-float)
                        (finish))
                      
                      (let ((e e1)
                            (m-turn 1))
                        (declare (type double-float e)
                                 (type fixnum m-turn))
                        ;; 利益の増分 delta が c 未満になるまでターンを進める
                        (iterate
                          (let ((delta 0.0d0))
                            (declare (type double-float delta))
                            ;; 配列を降順にスキャンし、eより大きい要素の差分のみを加算（Early Exit付き）
                            (iterate (for i from (1- k) downto 0)
                              (let ((val (aref arr i)))
                                (declare (type double-float val))
                                (if (> val e)
                                    (incf delta (- val e))
                                    (finish))))
                            (setf delta (* delta inv-k))
                            
                            (if (< delta c-float)
                                (progn
                                  ;; 限界効用がコストを下回ったため、直前のターンを最適とする
                                  (incf sum-p (- e (* c-float m-turn)))
                                  (finish))
                                (progn
                                  (incf e delta)
                                  (incf m-turn))))))))
                  (incf total-expected (* w sum-p))))))))
      
      (let ((ans (round total-expected)))
        (format t "Final ans = ~A~%" ans)
        ans))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating F(20)...
Final ans = 147668794

User time    =        3.128
System time  =        0.028
Elapsed time =        3.087
Allocation   = 4887320608 bytes
420 Page faults
GC time      =        0.275
 |------------------------------------------------------------|#
;;→ 147668794
:ok