;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "BCL-USER")

;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0910 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0910)

#||
Project Euler 910: L-expressions
数論的ショートカットとダブリング圧縮:
L式の評価は、関数 $v(y) = y^c(y+1)$ の高階反復適用に次元崩壊する。
c >= 9 であるため、初期値 d=678 (偶数) から生成される値は、
初手以降すべて 512 (2^9) の倍数に吸い込まれる。
これにより状態空間 10^9 は 10^9 / 512 = 1953125 個に圧縮される。
この圧縮空間上の写像をダブリングで事前計算することで O(N log B) の時間で
(B+1)^A 回の反復計算をバイパスする。
||#

(defconstant $const-mod 1000000000)
(defconstant $const-n 1953125)

(defun power-mod (base exp mod-val)
  "繰り返し二乗法による高速な剰余べき乗計算"
  (let ((res 1)
        (b (mod base mod-val)))
    (iterate (for p initially exp then (ash p -1))
      (while (> p 0))
      (when (oddp p)
        (setf res (mod (* res b) mod-val)))
      (setf b (mod (* b b) mod-val)))
    res))

(defun compose-tables (t-outer t-inner n)
  "2つの関数（写像テーブル）を合成する"
  (let ((t-out (make-array n :element-type 'fixnum)))
    (iterate (for z from 0 below n)
      (setf (aref t-out z)
            (aref t-outer (aref t-inner z))))
    t-out))

(defun make-pow-table (t-base k n)
  "ダブリングによる写像の K 回反復テーブルの作成"
  (let ((t-res (make-array n :element-type 'fixnum))
        (t-pow (make-array n :element-type 'fixnum)))
    (iterate (for z from 0 below n)
      (setf (aref t-res z) z)
      (setf (aref t-pow z) (aref t-base z)))
    (iterate (for power initially k then (ash power -1))
      (while (> power 0))
      (when (oddp power)
        (setf t-res (compose-tables t-pow t-res n)))
      (setf t-pow (compose-tables t-pow t-pow n)))
    t-res))

(defun make-tdc (n c mod-val)
  "初期関数 v_Dc(y) = y^c(y+1) の写像テーブルを作成"
  (let ((t-dc (make-array n :element-type 'fixnum)))
    (iterate (for z from 0 below n)
      (let* ((x (mod (* z 512) mod-val))
             (xc (power-mod x c mod-val))
             (val (mod (* xc (1+ x)) mod-val)))
        (setf (aref t-dc z) (floor val 512))))
    t-dc))

(defun make-db-f (t-f t-f-pow n mod-val)
  "関数 F から D_b(F) の写像テーブルを生成: v_Db(F)(x) = v_F^b( x * v_F(x) )"
  (let ((t-g (make-array n :element-type 'fixnum)))
    (iterate (for z from 0 below n)
      (let* ((x (mod (* z 512) mod-val))
             (z-v (aref t-f z))
             (v (mod (* z-v 512) mod-val))
             (y0 (mod (* x v) mod-val))
             (z-y0 (floor y0 512)))
        (setf (aref t-g z) (aref t-f-pow z-y0))))
    t-g))

(defun solve ()
  (let* ((a 12)
         (b 345678)
         (c 9012345)
         (d 678)
         (e 90)
         (v1 0) (y0 0) (val-x 0))
    
    (format t "Initializing base T_Dc table...~%")
    (let* ((t-dc (make-tdc $const-n c $const-mod))
           (t-dc-pow-b1 (make-pow-table t-dc (1+ b) $const-n)))
      
      ;; 1歩目：初期値 d は 512の倍数ではないため、個別に1手進める
      (setf v1 (mod (* (power-mod d c $const-mod) (1+ d)) $const-mod))
      (setf y0 (mod (* d v1) $const-mod))
      (setf val-x (* (aref t-dc-pow-b1 (floor y0 512)) 512))
      
      (format t "Computing first level functional map...~%")
      (let ((t-curr (make-db-f t-dc t-dc-pow-b1 $const-n $const-mod)))
        
        ;; a 回の反復
        (iterate (for i from 1 to a)
          (format t "Processing dimension level ~A / ~A...~%" i a)
          (let ((t-curr-pow-b (make-pow-table t-curr b $const-n)))
            (setf v1 val-x)
            (setf y0 (mod (* d v1) $const-mod))
            (setf val-x (* (aref t-curr-pow-b (floor y0 512)) 512))
            
            (when (< i a)
              (setf t-curr (make-db-f t-curr t-curr-pow-b $const-n $const-mod)))))
        
        (format t "Calculation complete. Formatting output...~%")
        (let ((result (mod (+ val-x e) $const-mod)))
          (format nil "~9,'0D" result))))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing base T_Dc table...
Computing first level functional map...
Processing dimension level 1 / 12...
Processing dimension level 2 / 12...
Processing dimension level 3 / 12...
Processing dimension level 4 / 12...
Processing dimension level 5 / 12...
Processing dimension level 6 / 12...
Processing dimension level 7 / 12...
Processing dimension level 8 / 12...
Processing dimension level 9 / 12...
Processing dimension level 10 / 12...
Processing dimension level 11 / 12...
Processing dimension level 12 / 12...
Calculation complete. Formatting output...

User time    =       42.432
System time  =        0.929
Elapsed time =       43.258
Allocation   = 10607671344 bytes
487508 Page faults
GC time      =        0.1073
 |------------------------------------------------------------|#
;;→ "547480666"
:ok