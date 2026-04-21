;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0184 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0184)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defun combinations-two (number)
  (if (< number 2)
      0
      (/ (* number (- number 1)) 2)))

(defun combinations-three (number)
  (if (< number 3)
      0
      (/ (* number (- number 1) (- number 2)) 6)))

(defun calculate-quadrant (x-coord y-coord)
  (cond ((and (> x-coord 0) (>= y-coord 0)) 1)
        ((and (<= x-coord 0) (> y-coord 0)) 2)
        ((and (< x-coord 0) (<= y-coord 0)) 3)
        ((and (>= x-coord 0) (< y-coord 0)) 4)
        (t 0)))

(defstruct (ray-vector (:conc-name ray-))
  (dx 0 :type integer)
  (dy 0 :type integer)
  (weight 0 :type integer)
  (quadrant 0 :type integer))

(defun ray-less-p (ray-a ray-b)
  (let ((quad-a (ray-quadrant ray-a))
        (quad-b (ray-quadrant ray-b)))
    (if (/= quad-a quad-b)
        (< quad-a quad-b)
        (let ((cross (- (* (ray-dx ray-a) (ray-dy ray-b))
                        (* (ray-dx ray-b) (ray-dy ray-a)))))
          (> cross 0)))))

(defun calculate-cross-product (ray-a ray-b)
  (- (* (ray-dx ray-a) (ray-dy ray-b))
     (* (ray-dx ray-b) (ray-dy ray-a))))

(defun calculate-dot-product (ray-a ray-b)
  (+ (* (ray-dx ray-a) (ray-dx ray-b))
     (* (ray-dy ray-a) (ray-dy ray-b))))

(defun solve (&optional (limit-radius 105))
  (let ((ray-hash (make-hash-table :test 'equal))
        (limit-radius-squared (* limit-radius limit-radius)))
    ;; Step 1: 勝義的整数化による格子点のRay（半直線）への還元
    (iterate
      (for x-coord from (- limit-radius) to limit-radius)
      (iterate
        (for y-coord from (- limit-radius) to limit-radius)
        (when (and (not (and (= x-coord 0) (= y-coord 0)))
                   (< (+ (* x-coord x-coord) (* y-coord y-coord)) limit-radius-squared))
          (let* ((common-divisor (gcd (abs x-coord) (abs y-coord)))
                 (reduced-dx (/ x-coord common-divisor))
                 (reduced-dy (/ y-coord common-divisor))
                 (key (cons reduced-dx reduced-dy)))
            (incf (gethash key ray-hash 0))))))

    (let* ((ray-list (iterate
                       (for (key-cons weight-val) in-hashtable ray-hash)
                       (collect (make-ray-vector :dx (car key-cons)
                                                 :dy (cdr key-cons)
                                                 :weight weight-val
                                                 :quadrant (calculate-quadrant (car key-cons) (cdr key-cons))))))
           ;; Step 2: 外積を用いた完全整数演算による偏角ソート
           (sorted-rays (sort (coerce ray-list 'vector) #'ray-less-p))
           (limit-ray-count (length sorted-rays))
           (doubled-rays (make-array (* 2 limit-ray-count)))
           (prefix-weights (make-array (1+ (* 2 limit-ray-count)) :initial-element 0)))

      ;; 周期境界条件のための配列2倍拡張
      (iterate
        (for index-ray from 0 below limit-ray-count)
        (setf (aref doubled-rays index-ray) (aref sorted-rays index-ray))
        (setf (aref doubled-rays (+ index-ray limit-ray-count)) (aref sorted-rays index-ray)))

      ;; O(1) 領域和のための累積和構築
      (iterate
        (for index-prefix from 0 below (* 2 limit-ray-count))
        (setf (aref prefix-weights (1+ index-prefix))
              (+ (aref prefix-weights index-prefix)
                 (ray-weight (aref doubled-rays index-prefix)))))

      (let* ((total-points (aref prefix-weights limit-ray-count))
             (total-triangles (combinations-three total-points))
             (sub-semi-circle 0)
             (sub-opposite 0)
             (right-index 1))

        ;; Step 3: 余事象の全単射的計数と尺取り法
        (iterate
          (for left-index from 0 below limit-ray-count)
          (let ((current-ray (aref doubled-rays left-index)))
            ;; 外積が正（反時計回りで180度未満）である限り右ポインタを進める
            (iterate
              (while (and (< right-index (+ left-index limit-ray-count))
                          (> (calculate-cross-product current-ray (aref doubled-rays right-index)) 0)))
              (incf right-index))

            (let* ((weight-h (- (aref prefix-weights right-index)
                                (aref prefix-weights (1+ left-index))))
                   (weight-left (ray-weight current-ray)))

              ;; 開半平面に収まる無効な三角形の組み合わせを加算
              (incf sub-semi-circle
                    (+ (* weight-left (combinations-two weight-h))
                       (* (combinations-two weight-left) weight-h)
                       (combinations-three weight-left)))

              ;; 原点を辺上に含む（対向する点が存在する）ケースの計数
              (when (and (< right-index (+ left-index limit-ray-count))
                         (= (calculate-cross-product current-ray (aref doubled-rays right-index)) 0)
                         (< (calculate-dot-product current-ray (aref doubled-rays right-index)) 0))
                (let ((index-opposite (mod right-index limit-ray-count)))
                  (when (< left-index index-opposite) ; 重複カウント防止
                    (let ((weight-opp (ray-weight (aref doubled-rays right-index))))
                      (incf sub-opposite
                            (+ (* weight-left weight-opp (- total-points weight-left weight-opp))
                               (* (combinations-two weight-left) weight-opp)
                               (* weight-left (combinations-two weight-opp)))))))))))

        ;; 観測用プリントデバッグ
        (format t "-----------------------------------------~%")
        (format t "Radius Constraint: ~A~%" limit-radius)
        (format t "Total Valid Points N: ~A~%" total-points)
        (format t "Distinct Rays R: ~A~%" limit-ray-count)
        (format t "Total Combinations C(N, 3): ~A~%" total-triangles)
        (format t "Subtracted (Semi-Circle): ~A~%" sub-semi-circle)
        (format t "Subtracted (Opposite Edge): ~A~%" sub-opposite)
        (format t "-----------------------------------------~%")
        
        (- total-triangles sub-semi-circle sub-opposite)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
-----------------------------------------
Radius Constraint: 105
Total Valid Points N: 34608
Distinct Rays R: 21000
Total Combinations C(N, 3): 6907813568656
Subtracted (Semi-Circle): 5178929707056
Subtracted (Opposite Edge): 3560237544
-----------------------------------------

User time    =        0.090
System time  =        0.006
Elapsed time =        0.055
Allocation   = 3822200 bytes
390 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1725323624056
:ok