;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0547 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0547)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【不変量と数論的ショートカットの証明】
任意の長方形内の距離の4重積分を S(w,h) という解析的数式に落とし込む。
さらに包除原理と積分の階差の法則により、任意の2つの長方形間の積分 I(A, B) は
4×4=16個の S(dx, dy) の加減算に O(1) で崩壊する。
これにより、シミュレーション空間（O(N^8)）の探索が、
定数回の行列参照のみによるマクロな方程式（O(N^4)）へと次元上昇を果たす。
||#

(defun make-double-float-array (rows cols)
  "2次元の倍精度浮動小数点配列を生成する"
  (make-array (list rows cols) :element-type 'double-float :initial-element 0.0d0))

(declaim (inline is-valid-lamina?))
(defun is-valid-lamina? (n w h x0 y0)
  "laminaの穴が正方形の内部に正しく収まっているかを判定する述語"
  (and (>= x0 1) (>= y0 1) (<= (+ x0 w) (1- n)) (<= (+ y0 h) (1- n))))

(defun precompute-s-matrix (n)
  "S(w,h) の値を 0~N まで事前計算して O(1) 参照可能にする"
  (let ((mat (make-double-float-array (1+ n) (1+ n))))
    (iterate ((w (scan-range :from 1 :upto n)))
      (iterate ((h (scan-range :from 1 :upto n)))
        (let* ((wf (coerce w 'double-float))
               (hf (coerce h 'double-float))
               (sq (sqrt (+ (* wf wf) (* hf hf))))
               (t1 (/ (- (+ (expt wf 5) (expt hf 5)) (expt sq 5)) 15.0d0))
               (t2 (/ (* wf wf hf hf sq) 3.0d0))
               (t3 (* (/ (* (expt wf 4) hf) 6.0d0) (log (/ (+ hf sq) wf))))
               (t4 (* (/ (* wf (expt hf 4)) 6.0d0) (log (/ (+ wf sq) hf)))))
          (setf (aref mat w h) (+ t1 t2 t3 t4)))))
    mat))

(declaim (inline compute-el))
(defun compute-el (n w h x0 y0 s-matrix)
  "1つのlamina L の期待距離 E_L を 16個の S(dx,dy) の足し合わせで O(1) で計算する"
  (declare (type fixnum n w h x0 y0)
           (type (simple-array double-float (* *)) s-matrix)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((dx0 (+ x0 w))
         (dx1 x0)
         (dx2 (- n dx0))
         (dx3 (- n dx1))
         (dy0 (+ y0 h))
         (dy1 y0)
         (dy2 (- n dy0))
         (dy3 (- n dy1))
         (i-ss (aref s-matrix n n))
         (i-hh (aref s-matrix w h))
         (i-sh 0.0d0))
    (declare (type fixnum dx0 dx1 dx2 dx3 dy0 dy1 dy2 dy3)
             (type double-float i-ss i-hh i-sh))
    
    ;; X軸・Y軸のそれぞれの差分と符号(cx, cy)の直積16パターン
    ;; cx = (1, -1, -1, 1), cy = (1, -1, -1, 1)
    (macrolet ((add-term (cx cy dx dy)
                 `(let ((val (aref s-matrix ,dx ,dy)))
                    ,(if (= (* cx cy) 1)
                         `(incf i-sh val)
                         `(decf i-sh val)))))
      ;; row 0 (cy=1)
      (add-term  1  1 dx0 dy0) (add-term -1  1 dx1 dy0) (add-term -1  1 dx2 dy0) (add-term  1  1 dx3 dy0)
      ;; row 1 (cy=-1)
      (add-term  1 -1 dx0 dy1) (add-term -1 -1 dx1 dy1) (add-term -1 -1 dx2 dy1) (add-term  1 -1 dx3 dy1)
      ;; row 2 (cy=-1)
      (add-term  1 -1 dx0 dy2) (add-term -1 -1 dx1 dy2) (add-term -1 -1 dx2 dy2) (add-term  1 -1 dx3 dy2)
      ;; row 3 (cy=1)
      (add-term  1  1 dx0 dy3) (add-term -1  1 dx1 dy3) (add-term -1  1 dx2 dy3) (add-term  1  1 dx3 dy3))

    (setf i-sh (* i-sh 0.25d0))

    ;; I(L, L) = I(S_out, S_out) - 2 I(S_out, H) + I(H, H)
    (let* ((i-ll (+ (- i-ss (* 2.0d0 i-sh)) i-hh))
           (area (- (* n n) (* w h)))
           (area-f (coerce area 'double-float)))
      (/ i-ll (* area-f area-f)))))

(defun solve (&optional (n 40))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((s-matrix (precompute-s-matrix n))
        (total 0.0d0))
    (declare (type double-float total))
    
    ;; 対称性を利用した実効演算回数の半減: w <= h に絞り、w < h の場合は結果を2倍する
    (iterate ((w (scan-range :from 1 :upto (- n 2))))
      (when (= (mod w 10) 0)
        (format t "観測: 穴幅 w=~D まで完了. 現在の期待値合計: ~,4F~%" w total))
      
      (iterate ((h (scan-range :from w :upto (- n 2))))
        (let ((mult (if (= w h) 1.0d0 2.0d0)))
          (declare (type double-float mult))
          (iterate ((x0 (scan-range :from 1 :upto (- n 1 w))))
            (iterate ((y0 (scan-range :from 1 :upto (- n 1 h))))
              (when (is-valid-lamina? n w h x0 y0)
                (incf total (* mult (compute-el n w h x0 y0 s-matrix)))))))))
    
    (let ((rounded-result (float (/ (round (* total 10000.0d0)) 10000.0d0) 0.0d0)))
      (format t "S(~D) = ~,4F~%" n rounded-result)
      rounded-result)))

#+| Do it | (project-euler-0547:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: 穴幅 w=10 まで完了. 現在の期待値合計: 7564620.2563
観測: 穴幅 w=20 まで完了. 現在の期待値合計: 10892148.2186
観測: 穴幅 w=30 まで完了. 現在の期待値合計: 11679963.0102
S(40) = 11730879.0023

User time    =        0.109
System time  =        0.011
Elapsed time =        0.074
Allocation   = 196670176 bytes
488 Page faults
GC time      =        0.005
 |------------------------------------------------------------|#
;;→ 1.17308790023D7
:ok
