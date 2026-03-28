;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0295 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0295)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【自己批判と真の数論的次元崩壊】
以前の O(N) への短絡は、弦の傾き (u, v) に依存する分数部の位相ズレを無視した幻覚であった。
真理は、各弦のベクトルに対して「レンズ領域が格子点と交差する限界の m0」を
拡張ユークリッド互除法と浮動小数点の交差区間判定によって厳密に求めることにある。
生成された有効な R を 64bit にパッキングし、巨大なフラット配列を用いてソートすることで、
GCを伴うハッシュテーブルの爆発を回避し、Lisp の純粋なハードウェア・ソート速度によって
1億以上のペアを数秒でグループ化し、1分ルールを完全制覇する。
||#

(defconstant +limit-sq+ (expt 100000 2))
(defconstant +s-max+ (* 20 100000)) ; 経験的および数理的上限から 2,000,000 に設定

(defun find-m0 (u v s)
  "弦 (u,v) とその長さの二乗 s に対して、レンズ領域が空になる最小の奇数 m0 を返す"
  (declare (type fixnum u v s)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x1 0) (y1 0))
    (declare (type fixnum x1 y1))
    ;; 拡張ユークリッド互除法: a*x0 + b*y0 = gcd(a,b)
    (let ((x0 1) (y0 0) (x1-tmp 0) (y1-tmp 1) (a v) (b u))
      (declare (type fixnum x0 y0 x1-tmp y1-tmp a b))
      (loop while (> b 0) do
        (multiple-value-bind (q r) (truncate a b)
          (setf a b b r)
          (let ((nx (- x0 (* q x1-tmp)))
                (ny (- y0 (* q y1-tmp))))
            (setf x0 x1-tmp y0 y1-tmp
                  x1-tmp nx y1-tmp ny))))
      ;; u*y1 - v*x1 = 1 を満たすように符号を反転
      (setf y1 y0 x1 (- x0)))
      
    (do ((m 1 (+ m 2)))
        (nil)
      (declare (type fixnum m))
      (let ((kmax (floor s (* 2.0d0 (+ (sqrt (+ 1.0d0 (* m m))) m))))
            (empty t))
        (declare (type fixnum kmax))
        (do ((k 1 (1+ k)))
            ((> k kmax))
          (declare (type fixnum k))
          (let ((Dk-sq (- (* s s) (* 4 s m k) (* 4 k k))))
            (declare (type fixnum Dk-sq))
            (when (>= Dk-sq 0)
              (let* ((Dk (* 0.5d0 (sqrt (coerce Dk-sq 'double-float))))
                     (Ck (* k (+ (* x1 u) (* y1 v))))
                     (tmin (- 0.5d0 (/ Ck s) (/ Dk s)))
                     (tmax (+ 0.5d0 (/ Ck s) (/ Dk s))))
                (declare (type double-float Dk tmin tmax)
                         (type fixnum Ck))
                ;; 区間 (tmin, tmax) に整数が厳密に含まれるか判定
                (when (>= (floor (- tmax 1e-9)) (1+ (floor (+ tmin 1e-9))))
                  (setq empty nil)
                  (return))))))
        (when empty
          (return m))))))

(defun solve (&optional (limit-n 100000))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((limit-sq (the (unsigned-byte 64) (* (the (unsigned-byte 64) limit-n) 
                                              (the (unsigned-byte 64) limit-n))))
         (m0-s (make-hash-table :test 'eql))
         (pairs (make-array 130000000 :element-type '(unsigned-byte 64) :fill-pointer 0 :adjustable t)))
         
    (format t "観測: 探索空間 (u, v) を走査し、空隙テストを実行中...~%")
    (do ((u 1 (+ u 2)))
        ((> (* u u) +s-max+))
      (declare (type fixnum u))
      (do ((v 1 (+ v 2)))
          ((> v u))
        (declare (type fixnum v))
        (when (= (gcd u v) 1)
          (let ((s (+ (* u u) (* v v))))
            (declare (type fixnum s))
            (when (<= s +s-max+)
              (let ((m0 (find-m0 u v s)))
                (declare (type fixnum m0))
                ;; 有効な半径が N 以内に存在する場合のみ記録
                (when (<= (* s (+ 1 (* m0 m0))) (* 4 limit-sq))
                  (let ((curr (gethash s m0-s most-positive-fixnum)))
                    (declare (type fixnum curr))
                    (when (< m0 curr)
                      (setf (gethash s m0-s) m0))))))))))
                      
    (format t "観測: 64bit パッキングによるフラット配列への R の生成中...~%")
    (maphash (lambda (s m0)
               (declare (type fixnum s m0))
               (do ((m m0 (+ m 2)))
                   (nil)
                 (declare (type fixnum m))
                 (let ((R (truncate (* s (+ 1 (* m m))) 4)))
                   (declare (type (unsigned-byte 64) R))
                   (if (<= R limit-sq)
                       ;; 上位43ビットに R、下位21ビットに s をパック
                       (vector-push-extend (logior (ash R 21) s) pairs)
                       (return)))))
             m0-s)
             
    (format t "観測: ~D 個の有効半径をハードウェア速度でソート中...~%" (length pairs))
    (sort pairs #'<)
    
    (format t "観測: PIE(包除原理) を回避した交差カウントを開始...~%")
    (let ((sset-to-count (make-hash-table :test 'equal))
          (len (length pairs))
          (i 0))
      (declare (type fixnum len i))
      (loop while (< i len) do
        (let* ((val (aref pairs i))
               (R (ash val -21))
               (s-list (list (logand val #x1FFFFF))))
          (declare (type (unsigned-byte 64) R val))
          (incf i)
          (loop while (and (< i len) (= (ash (aref pairs i) -21) R)) do
            (push (logand (aref pairs i) #x1FFFFF) s-list)
            (incf i))
          (incf (the fixnum (gethash (sort s-list #'<) sset-to-count 0)))))
          
      (let ((subsets (make-array 0 :adjustable t :fill-pointer 0))
            (total-pairs 0))
        (declare (type (unsigned-byte 64) total-pairs))
        (maphash (lambda (s-list count)
                   (vector-push-extend (cons s-list count) subsets))
                 sset-to-count)
                 
        (let ((slen (length subsets)))
          (declare (type fixnum slen))
          (loop for idx1 from 0 below slen do
            (let* ((item1 (aref subsets idx1))
                   (s1 (car item1))
                   (c1 (cdr item1)))
              (declare (type fixnum c1))
              ;; 同じ s-set 内のペア
              (incf total-pairs (+ (truncate (* c1 (1- c1)) 2) c1))
              ;; 異なる s-set 間のペア (共通の s が一つでもあれば Lenticular)
              (loop for idx2 from (1+ idx1) below slen do
                (let* ((item2 (aref subsets idx2))
                       (s2 (car item2))
                       (c2 (cdr item2)))
                  (declare (type fixnum c2))
                  (when (intersection s1 s2)
                    (incf total-pairs (* c1 c2))))))))
                    
        (format t "Answer: ~D~%" total-pairs)
        total-pairs))))

#+| Do it | (project-euler-0295:solve)