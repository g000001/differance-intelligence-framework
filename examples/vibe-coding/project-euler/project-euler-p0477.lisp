;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0477 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0477)

(defun solve (&optional (N 100000000))
  (declare (type fixnum N))
  (format t "Calculating F(~A)...~%" N)
  
  ;; 乱数シーケンスの性質上、スタックサイズは最大 N/3 程度まで成長する可能性があるため
  ;; 巨大な単一フラット配列を確保する。
  ;; これによりループ内での動的なメモリアロケーション(GC)を完全に排除する。
  (let ((stack (make-array N :element-type 'fixnum))
        (sp 0)     ;; スタックポインタ
        (s 0)      ;; シーケンスの現在値
        (total 0)) ;; 全要素の合計
    (declare (type (simple-array fixnum (*)) stack)
             (type fixnum sp s total))
             
    (iterate (for i from 1 to N)
      (incf total s)
      (setf (aref stack sp) s)
      (incf sp)
      
      ;; 還元のループ： x <= y >= z を満たす限り x - y + z に置換
      (iterate
        (when (< sp 3) (finish))
        (let ((z (aref stack (- sp 1)))
              (y (aref stack (- sp 2)))
              (x (aref stack (- sp 3))))
          (declare (type fixnum x y z))
          (if (and (<= x y) (>= y z))
              (progn
                (decf sp 3)
                (setf (aref stack sp) (+ (- x y) z))
                (incf sp))
              (finish))))
              
      ;; 次の要素の生成: s = (s^2 + 45) mod 1000000007
      ;; fixnum (61bit) は 10^18 まで安全に格納できるため (* s s) はオーバーフローしない
      (let ((s-sq (* s s)))
        (declare (type fixnum s-sq))
        (setf s (mod (+ s-sq 45) 1000000007)))
        
      ;; デバッグ用中間ログ
      (when (zerop (mod i 20000000))
        (format t "Processed ~A / ~A items...~%" i N)))
        
    (format t "Reduction complete. Stack size: ~A~%" sp)
    
    ;; 最終的にスタックには「谷（Valley）」状のシーケンスが残る。
    ;; 谷の最適戦略は「常に両端の大きい方を取る」ことであるため、
    ;; O(N log N) のソートを避け、O(K) のマージ（両端からの双方向ポインタ）で
    ;; スコアの差分 (diff) を計算する。
    (let ((diff 0)
          (left 0)
          (right (1- sp))
          (sign 1))
      (declare (type fixnum diff left right sign))
      
      (iterate (while (<= left right))
        (let ((val-l (aref stack left))
              (val-r (aref stack right)))
          (declare (type fixnum val-l val-r))
          (if (>= val-l val-r)
              (progn
                (incf diff (* sign val-l))
                (incf left))
              (progn
                (incf diff (* sign val-r))
                (decf right)))
          (setf sign (- sign))))
          
      ;; 先手(Player 1)のスコア = (総和 + スコア差) / 2
      (let ((ans (ash (+ total diff) -1)))
        (format t "Final ans = ~A~%" ans)
        ans))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating F(100000000)...
Processed 20000000 / 100000000 items...
Processed 40000000 / 100000000 items...
Processed 60000000 / 100000000 items...
Processed 80000000 / 100000000 items...
Processed 100000000 / 100000000 items...
Reduction complete. Stack size: 10
Final ans = 25044905874565165

User time    =       10.417
System time  =        0.035
Elapsed time =       10.344
Allocation   = 800158832 bytes
408 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 25044905874565165
:ok
