;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0966 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0966)

(declaim (inline cross-product))
(defun cross-product (x1 y1 x2 y2)
  (- (* x1 y2) (* x2 y1)))

(declaim (inline normalize-angle))
(defun normalize-angle (ang)
  (let ((a (mod ang (* 2.0d0 pi))))
    (if (> a pi) (- a (* 2.0d0 pi))
        (if (< a (- pi)) (+ a (* 2.0d0 pi)) a))))

(defun wedge-area (ax ay bx by R)
  "原点を中心とする半径Rの円と、原点・A・Bがなす三角形の交差面積（符号付き）を計算する"
  (let* ((dx (- bx ax))
         (dy (- by ay))
         (a-coef (+ (* dx dx) (* dy dy)))
         (b-coef (* 2.0d0 (+ (* ax dx) (* ay dy))))
         (c-coef (- (+ (* ax ax) (* ay ay)) (* R R)))
         (delta (- (* b-coef b-coef) (* 4.0d0 a-coef c-coef)))
         (t1 -1.0d0) (t2 -1.0d0))
    
    (when (>= delta 0.0d0)
      (let ((sqrt-delta (sqrt delta)))
        (setf t1 (/ (- (- b-coef) sqrt-delta) (* 2.0d0 a-coef)))
        (setf t2 (/ (+ (- b-coef) sqrt-delta) (* 2.0d0 a-coef)))))
    
    ;; 線分ABと円の交差パラメータを [0, 1] にクランプ
    (let* ((u1 (max 0.0d0 (min 1.0d0 t1)))
           (u2 (max 0.0d0 (min 1.0d0 t2)))
           (area 0.0d0))
      
      (labels ((add-sector (p1x p1y p2x p2y)
                 (let ((ang (normalize-angle (- (atan p2y p2x) (atan p1y p1x)))))
                   (incf area (* 0.5d0 R R ang))))
               (add-triangle (p1x p1y p2x p2y)
                 (incf area (* 0.5d0 (cross-product p1x p1y p2x p2y)))))
        
        ;; [0, u1]: 円弧
        (when (> u1 0.0d0)
          (add-sector ax ay (+ ax (* u1 dx)) (+ ay (* u1 dy))))
        ;; [u1, u2]: 三角形内部
        (when (> u2 u1)
          (add-triangle (+ ax (* u1 dx)) (+ ay (* u1 dy)) (+ ax (* u2 dx)) (+ ay (* u2 dy))))
        ;; [u2, 1]: 円弧
        (when (< u2 1.0d0)
          (add-sector (+ ax (* u2 dx)) (+ ay (* u2 dy)) bx by)))
      area)))

(defun circle-triangle-area (cx cy R tx1 ty1 tx2 ty2 tx3 ty3)
  "三角形と円の交差面積をウェッジ積分の和として厳密に求める"
  (+ (wedge-area (- tx1 cx) (- ty1 cy) (- tx2 cx) (- ty2 cy) R)
     (wedge-area (- tx2 cx) (- ty2 cy) (- tx3 cx) (- ty3 cy) R)
     (wedge-area (- tx3 cx) (- ty3 cy) (- tx1 cx) (- ty1 cy) R)))

(defun calculate-I-robust (a-int b-int c-int)
  (let* ((a (coerce a-int 'double-float))
         (b (coerce b-int 'double-float))
         (c (coerce c-int 'double-float))
         ;; Kahan's formula による面積の高精度計算
         (A-area (* 0.25d0 (sqrt (max 0.0d0 (* (+ c (+ b a)) (- a (- c b)) (+ a (- c b)) (+ c (- b a)))))))
         (R (sqrt (/ A-area pi)))
         ;; 頂点座標の配置 (Cを原点, aをx軸)
         (tx1 0.0d0) (ty1 0.0d0)
         (tx2 a)     (ty2 0.0d0)
         (cosC (/ (- (+ (* a a) (* b b)) (* c c)) (* 2.0d0 a b)))
         (sinC (sqrt (max 0.0d0 (- 1.0d0 (* cosC cosC)))))
         (tx3 (* b cosC)) (ty3 (* b sinC))
         ;; 初期探索点は内心 (Incenter)
         (perimeter (+ a b c))
         (cx (/ (+ (* a tx3) (* b tx2) (* c tx1)) perimeter))
         (cy (/ (+ (* a ty3) (* b ty2) (* c ty1)) perimeter))
         (best-area 0.0d0)
         (step (/ R 2.0d0)))
    
    ;; 局所探索 (Hill Climbing): 内心から開始し、交差面積の極大値へ登る
    (iterate (for iter from 0 to 50)
      (let ((moved nil))
        (iterate (for dx in '(-1.0d0 0.0d0 1.0d0))
          (iterate (for dy in '(-1.0d0 0.0d0 1.0d0))
            (when (or (/= dx 0.0d0) (/= dy 0.0d0))
              (let* ((nx (+ cx (* dx step)))
                     (ny (+ cy (* dy step)))
                     (area (abs (circle-triangle-area nx ny R tx1 ty1 tx2 ty2 tx3 ty3))))
                (when (> area best-area)
                  (setf best-area area)
                  (setf cx nx)
                  (setf cy ny)
                  (setf moved t))))))
        (unless moved
          (setf step (* step 0.5d0)))))
    best-area))

(defun solve (&optional (limit 200))
  (format t "Starting Universal Dimensional Collapse (Green's Theorem & Hill Climbing) for Limit=~A...~%" limit)
  (let ((sum 0.0d0)
        (count 0))
    ;; naive なループで、境界の取りこぼしを完全に排除
    (iterate (for a from 1 to limit)
      (iterate (for b from a to limit)
        (iterate (for c from b to (1- (+ a b)))
          (when (<= (+ a b c) limit)
            (incf count)
            (incf sum (calculate-I-robust a b c))))))
    
    (format t "Processed ~A valid triangles.~%" count)
    ;; 小数点以下2桁への標準的な四捨五入 (half-up)
    (let* ((rounded-sum (/ (round (* sum 100.0d0)) 100.0d0))
           (ans-str (format nil "~,2f" rounded-sum)))
      (format t "Finished. Answer: ~A~%" ans-str)
      ans-str)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Universal Dimensional Collapse (Green's Theorem & Hill Climbing) for Limit=200...
Processed 57222 valid triangles.
Finished. Answer: 29337152.09

User time    =  0:01:23.442
System time  =        1.220
Elapsed time =  0:01:38.315
Allocation   = 76362554736 bytes
3417 Page faults
GC time      =        0.676
 |------------------------------------------------------------|#
;;→ "29337152.09"
:ok