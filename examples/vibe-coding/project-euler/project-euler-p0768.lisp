;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0768 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0768)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【不変量と数論的ショートカットの証明】
配置多項式が円分多項式のイデアルに含まれる条件を利用し、360の空間を12個の独立な 2x3x5 空間に分解する。
さらに、2x3x5 空間におけるバランス条件を「Zスライスの第2階差分シグネチャ D が一致すること」へ還元。
各シグネチャ S について、重み w の母関数 P_S(t) を求め、g(t) = sum_S (P_S(t))^5 を計算する。
最後に全体の母関数 G(t) = (g(t))^12 の t^20 の係数を抽出することで、
O(2^360) の問題を事実上の O(1) （最大360次の多項式乗算）へと次元上昇させる。
||#

(defun make-integer-array (size &key (initial-element 0))
  (make-array size :element-type 'integer :initial-element initial-element))

(declaim (ftype (function ((simple-array integer (*)) (simple-array integer (*)))
                          (simple-array integer (*)))
                poly-mul poly-add))

(defun poly-mul (p1 p2)
  "多項式 p1 と p2 を乗算する"
  (declare (type (simple-array integer (*)) p1 p2))
  (let* ((len1 (length p1))
         (len2 (length p2))
         (res (make-integer-array (+ len1 len2 -1))))
    (iterate ((i (scan-range :length len1)))
      (let ((v1 (aref p1 i)))
        (when (not (zerop v1))
          (iterate ((j (scan-range :length len2)))
            (incf (aref res (+ i j)) (* v1 (aref p2 j)))))))
    res))

(defun poly-add (p1 p2)
  "多項式 p1 と p2 を加算する"
  (declare (type (simple-array integer (*)) p1 p2))
  (let* ((len1 (length p1))
         (len2 (length p2))
         (max-len (max len1 len2))
         (res (make-integer-array max-len)))
    (iterate ((i (scan-range :length len1)))
      (incf (aref res i) (aref p1 i)))
    (iterate ((i (scan-range :length len2)))
      (incf (aref res i) (aref p2 i)))
    res))

(defun poly-pow (p n)
  "多項式 p の n 乗を計算する"
  (declare (type (simple-array integer (*)) p)
           (type fixnum n))
  (let ((res (make-integer-array 1 :initial-element 1)))
    (iterate ((i (scan-range :length n)))
      (setf res (poly-mul res p)))
    res))

(declaim (inline pattern-w pattern-d))
(defun pattern-w (p)
  "2x3パターンの重み(1の数)を返す"
  (declare (type fixnum p))
  (logcount p))

(defun pattern-d (p)
  "2x3パターンのシグネチャ D=(d1, d2) を計算する"
  (declare (type fixnum p))
  (let ((a00 (logand (ash p 0) 1))
        (a01 (logand (ash p -1) 1))
        (a02 (logand (ash p -2) 1))
        (a10 (logand (ash p -3) 1))
        (a11 (logand (ash p -4) 1))
        (a12 (logand (ash p -5) 1)))
    (declare (type fixnum a00 a01 a02 a10 a11 a12))
    (let ((d1 (the fixnum (+ (the fixnum (- a00 a10 a01)) a11)))
          (d2 (the fixnum (+ (the fixnum (- a01 a11 a02)) a12))))
      (cons d1 d2))))

(defun build-g ()
  "1つの 2x3x5 独立空間において、バランスする配置の母関数 g(t) を構築する"
  (let ((s-hash (make-hash-table :test 'equal)))
    ;; 1. 64パターンの分類
    (iterate ((p (scan-range :length 64)))
      (let ((w (pattern-w p))
            (s (pattern-d p)))
        (unless (gethash s s-hash)
          (setf (gethash s s-hash) (make-integer-array 7)))
        (incf (aref (gethash s s-hash) w))))
    ;; 2. 各シグネチャ S ごとに P_S(t)^5 を足し合わせる
    (let ((g (make-integer-array 1)))
      (iterate (((s poly) (scan-hash s-hash)))
        (setf g (poly-add g (poly-pow poly 5))))
      g)))

(defun solve ()
  (format t "観測: g(t) の構築を開始~%")
  (let* ((g (build-g))
         ;; 3. 12個の独立空間を合成するため G(t) = g(t)^12 を計算
         (g-pow12 (poly-pow g 12)))
    (format t "観測: G(t) = g(t)^12 の計算完了. 最大到達次数 = ~D~%" (1- (length g-pow12)))
    ;; 4. キャンドルが 20 個の場合の組み合わせ数を抽出
    (let ((ans (aref g-pow12 20)))
      (format t "f(360, 20) = ~D~%" ans)
      ans)))

#+| Do it | (project-euler-0768:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: g(t) の構築を開始
観測: G(t) = g(t)^12 の計算完了. 最大到達次数 = 360
f(360, 20) = 14655308696436060

User time    =        0.021
System time  =        0.001
Elapsed time =        0.011
Allocation   = 2326424 bytes
115 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 14655308696436060
:ok