;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0165 (:use cl alexandria))
(in-package #:project-euler-0165)

;; ============================================================
;; 1. 世俗諦：Blum Blum Shub 擬似乱数生成器の実装
;; ============================================================

(defun generate-segments (n)
  "BBS生成器を用いてn本の線分を生成する。
   各線分は (x1 y1 x2 y2) の fixnum リストとして表現される。"
  (let ((s 290797)
        (m 50515093)
        (segments (make-array n)))
    (loop for i from 0 below n do
         (let ((coords (loop repeat 4 do
                            (setf s (mod (* s s) m))
                            collect (mod s 500))))
           (setf (aref segments i) coords)))
    segments))

;; ============================================================
;; 2. 勝義諦：有理数による厳密な交差判定（ACX Jump）
;; ============================================================

(defun get-true-intersection (s1 s2)
  "2つの線分s1, s2の真の交点を求める。
   交点が存在する場合は (x . y) の有理数ペアを返し、存在しない場合はnilを返す。
   NMF（非中道の誤謬）を避けるため、有理数演算で精度を担保する。"
  (destructuring-bind (x1 y1 x2 y2) s1
    (destructuring-bind (x3 y3 x4 y4) s2
      (let* ((dx1 (- x2 x1))
             (dy1 (- y2 y1))
             (dx2 (- x4 x3))
             (dy2 (- y4 y3))
             (det (- (* dx1 dy2) (* dy1 dx2))))
        ;; 行列式 det = 0 の場合、線分は平行（交点なしか無限個）
        (unless (zerop det)
          (let ((u (/ (- (* (- x3 x1) dy2) (* (- y3 y1) dx2)) det))
                (v (/ (- (* (- x3 x1) dy1) (* (- y3 y1) dx1)) det)))
            ;; 真の交点の条件： 0 < u < 1 かつ 0 < v < 1 (端点を含まない内部点)
            (when (and (> u 0) (< u 1) (> v 0) (< v 1))
              ;; 交点の座標を計算（有理数として保持）
              (cons (+ x1 (* u dx1))
                    (+ y1 (* u dy1))))))))))

;; ============================================================
;; 3. 中道の現成：探索と集計
;; ============================================================

(defun solve-problem-165 ()
  "5000本の線分から重複のない真の交点の総数を求める。"
  (let* ((n 5000)
         (segments (generate-segments n))
         ;; 重複を排除するため、有理数ペアをキーとするハッシュテーブルを使用
         (intersection-points (make-hash-table :test 'equal)))
    (format t "Calculating true intersections for ~A segments...~%" n)
    (loop for i from 0 below (1- n) do
         (when (zerop (mod i 500))
           (format t "Processing segment ~A...~%" i))
         (loop for j from (1+ i) below n do
              (let ((pt (get-true-intersection (aref segments i) (aref segments j))))
                (when pt
                  (setf (gethash pt intersection-points) t)))))
    (let ((result (hash-table-count intersection-points)))
      (format t "Found ~A distinct true intersection points.~%" result)
      result)))

;; 実行用エントリーポイント
(defun main ()
  (time (solve-problem-165)))

;; (main)
#+| Do it | (main )
;;; ▻ Calculating true intersections for 5000 segments...
;;; ▻ Processing segment 0...
;;; ▻ Processing segment 500...
;;; ▻ Processing segment 1000...
;;; ▻ Processing segment 1500...
;;; ▻ Processing segment 2000...
;;; ▻ Processing segment 2500...
;;; ▻ Processing segment 3000...
;;; ▻ Processing segment 3500...
;;; ▻ Processing segment 4000...
;;; ▻ Processing segment 4500...
;;; ▻ Found 2868868 distinct true intersection points.
;;; → 2868868

:ok

