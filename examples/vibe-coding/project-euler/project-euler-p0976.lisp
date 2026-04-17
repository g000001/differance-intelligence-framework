;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0976 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0976)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)

(defconstant +mod+ 1234567891)
(defconstant +inv2+ 617283946) ; (1234567891 + 1) / 2

(defun mod-add (num-a num-b)
  (declare (type (unsigned-byte 62) num-a num-b))
  (let ((sum-val (+ num-a num-b)))
    (if (>= sum-val +mod+) (- sum-val +mod+) sum-val)))

(defun mod-sub (num-a num-b)
  (declare (type (unsigned-byte 62) num-a num-b))
  (let ((diff-val (- num-a num-b)))
    (if (< diff-val 0) (+ diff-val +mod+) diff-val)))

(defun mod-mul (num-a num-b)
  (declare (type (unsigned-byte 62) num-a num-b))
  (mod (* num-a num-b) +mod+))

(defun solve ()
  (let* (($limit #.(expt 10 7))
         ($n $limit)
         ;; バケツの要素数を計算
         ($n1 (floor (+ $n 3) 4))
         ($n3 (floor (+ $n 1) 4))
         ($ne (floor $n 2))
         
         ;; 母関数のパラメータ
         ($u1 $n)
         ($v1 $ne)
         ($u2 (+ $n1 $ne))
         ($v2 (+ $n3 $ne))
         
         ;; 漸化式用の定数
         ($diff-1 (mod-sub (mod $u1 +mod+) (mod $v1 +mod+)))
         ($sum-1  (mod-add (mod $u1 +mod+) (mod $v1 +mod+)))
         ($diff-2 (mod-sub (mod $u2 +mod+) (mod $v2 +mod+)))
         ($sum-2  (mod-add (mod $u2 +mod+) (mod $v2 +mod+)))
         
         ($ans 0)
         ($inv-array (make-array (1+ $limit) :element-type '(unsigned-byte 62))))
    
    (format t "Step 1: O(K) 逆元テーブルの構築中...~%")
    (setf (aref $inv-array 1) 1)
    (iterate (for i from 2 to $limit)
             (setf (aref $inv-array i)
                   (mod-sub 0 (mod-mul (floor +mod+ i)
                                       (aref $inv-array (mod +mod+ i))))))
    
    (format t "Step 2: 母関数の微分漸化式による計算を実行中...~%")
    (let ((p-prev 1)
          (p-curr $diff-1)
          (q-prev 1)
          (q-curr $diff-2)
          (c-curr (mod $n +mod+)))
      
      ;; k = 1 の初期処理
      (let ((e-1 (mod-mul (mod-add p-curr q-curr) +inv2+)))
        (setf $ans (mod-add $ans e-1)))
      
      (iterate (for k from 2 to $limit)
               (let* ((inv-k (aref $inv-array k))
                      
                      ;; p_k の漸化式計算
                      (term1-1 (mod-mul $diff-1 p-curr))
                      (term1-2 (mod-mul (+ $sum-1 (- k 2)) p-prev))
                      (p-next (mod-mul (mod-add term1-1 term1-2) inv-k))
                      
                      ;; q_k の漸化式計算
                      (term2-1 (mod-mul $diff-2 q-curr))
                      (term2-2 (mod-mul (+ $sum-2 (- k 2)) q-prev))
                      (q-next (mod-mul (mod-add term2-1 term2-2) inv-k))
                      
                      ;; 総組み合わせ C_k の更新
                      (c-next (mod-mul c-curr (mod-mul (+ $n k -1) inv-k)))
                      
                      ;; 相殺ペア成分 E_k
                      (e-k (mod-mul (mod-add p-next q-next) +inv2+)))
                 
                 (if (evenp k)
                     (setf $ans (mod-add $ans (mod-sub c-next e-k)))
                     (setf $ans (mod-add $ans e-k)))
                 
                 ;; 状態のシフト
                 (setf p-prev p-curr)
                 (setf p-curr p-next)
                 (setf q-prev q-curr)
                 (setf q-curr q-next)
                 (setf c-curr c-next))))
    
    (format t "Final Result P(~A, ~A): ~A~%" $limit $limit $ans)
    $ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: O(K) 逆元テーブルの構築中...
Step 2: 母関数の微分漸化式による計算を実行中...
Final Result P(10000000, 10000000): 675608326

User time    =        2.479
System time  =        0.047
Elapsed time =        2.471
Allocation   = 93089976 bytes
20115 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 675608326
:ok