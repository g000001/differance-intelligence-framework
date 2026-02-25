
;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0264 (:use cl alexandria))
;;; (in-package #:project-euler-0264)

;;; ;; ==============================================================================
;;; ;; Project Euler 0264: Triangle Centres
;;; ;; ------------------------------------------------------------------------------
;;; ;; This solution employs the Two-Truths Entanglement (二諦随伴) protocol.
;;; ;; 1. Emptiness (Ultimate Truth): The geometric properties of the Euler line.
;;; ;;    For any triangle with circumcentre O(0,0), the orthocenter H satisfies
;;; ;;    H = A + B + C. Given H(5,0), we have xA + xB + xC = 5 and yA + yB + yC = 0.
;;; ;; 2. Form (Conventional Truth): Lattice points on a circle of radius R.
;;; ;;    We iterate through lattice points (x3, y3) to find valid triangles.
;;; ;; ==============================================================================


;;; (defconstant +sq-mask-64+
;;;   (let ((mask 0))
;;;     (loop for i from 0 to 63 do (setf mask (logior mask (ash 1 (mod (* i i) 64)))))
;;;     mask)
;;;   "64を法とする平方数のビットマスク。isqrtの無駄打ちを防ぐガードレール。")

;;; (defun solve-project-euler-0264 ()
;;;   "Find the sum of perimeters of triangles with circumcentre (0,0) and orthocenter (5,0)."
;;;   (let ((total-sum 0.0d0)
;;;         (limit 100000.0d0)
;;;         (max-r 20000))
;;;     (declare (optimize (speed 3) (safety 0) (debug 0))
;;;              (type double-float total-sum limit)
;;;              (type fixnum max-r))
;;;     
;;;     ;; Iterate through all possible lattice points for one vertex P3(x3, y3).
;;;     ;; The maximum radius is bounded by the perimeter limit.
;;;     (loop
;;;        for x3 from (- max-r) to max-r
;;;        do (loop
;;;              for y3 from (- max-r) to max-r do
;;;                (let* ((x3-5 (- x3 5))
;;;                       (c (+ (* x3-5 x3-5) (* y3 y3))))
;;;                  (declare (type fixnum x3-5 c))
;;;                  ;; c = |P3 - H|^2. c=0 is a singularity handled separately.
;;;                  (when (> c 0)
;;;                    (let* ((k (+ (* x3 x3) (* y3 y3)))
;;;                           ;; m2 = c * (4K - c) is the discriminant for the intersection
;;;                           ;; of the circle and the line derived from the orthocenter property.
;;;                           (m2 (* (the (signed-byte 64) c)
;;;                                  (the (signed-byte 64) (- (* 4 k) c)))))
;;;                      (declare (type (signed-byte 64) k m2))
;;;                      (when (and (>= m2 0)
;;;                                 (logbitp (logand m2 63) +sq-mask-64+)) ; 平方数の可能性がないものをO(1)で弾く
;;;                        (let ((m (isqrt m2)))
;;;                          (declare (type (signed-byte 64) m))
;;;                          ;; Check if the discriminant is a perfect square (Potential for lattice points).
;;;                          (when (= (* m m) m2)
;;;                            (let* ((2c (* 2 c))
;;;                                   ;; Formulas for the coordinates of P1 and P2 based on P3 and H.
;;;                                   (num1-x (+ (* (- 5 x3) c) (* y3 m)))
;;;                                   (num1-y (+ (* (- y3) c) (* (- 5 x3) m))))
;;;                              ;; Check if the coordinates are integers.
;;;                              (when (and (zerop (mod num1-x 2c))
;;;                                         (zerop (mod num1-y 2c)))
;;;                                (let* ((x1 (truncate num1-x 2c))
;;;                                       (y1 (truncate num1-y 2c))
;;;                                       (x2 (- 5 x3 x1))
;;;                                       (y2 (- 0 y3 y1)))
;;;                                  ;; Ensure the triangle is non-degenerate (vertices are distinct).
;;;                                  (unless (or (and (= x1 x3) (= y1 y3))
;;;                                              (and (= x1 x2) (= y1 y2))
;;;                                              (and (= x2 x3) (= y2 y3)))
;;;                                    (let ((p (+ (sqrt (float (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))) 1.0d0))
;;;                                                (sqrt (float (+ (* (- x2 x3) (- x2 x3)) (* (- y2 y3) (- y2 y3))) 1.0d0))
;;;                                                (sqrt (float (+ (* (- x3 x1) (- x3 x1)) (* (- y3 y1) (- y3 y1))) 1.0d0)))))
;;;                                      (when (<= p limit)
;;;                                        (setf total-sum (+ total-sum p))))))))))))))))
;;;     
;;;     ;; Handle the case where one vertex is H(5,0) (the singularity c=0).
;;;     ;; This happens when K = 25.
;;;     (let ((c0-sum 0.0d0))
;;;       (let ((pts '((5 0) (-5 0) (0 5) (0 -5) (3 4) (3 -4) (-3 4) (-3 -4) (4 3) (4 -3) (-4 3) (-4 -3))))
;;;         (dolist (p1 pts)
;;;           (let ((x1 (car p1)) (y1 (cadr p1))
;;;                 (x3 5) (y3 0))
;;;             (let ((x2 (- 0 x1)) (y2 (- 0 y1)))
;;;               (unless (or (and (= x1 x3) (= y1 y3))
;;;                           (and (= x1 x2) (= y1 y2))
;;;                           (and (= x2 x3) (= y2 y3)))
;;;                 (let ((p (+ (sqrt (float (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))) 1.0d0))
;;;                             (sqrt (float (+ (* (- x2 x3) (- x2 x3)) (* (- y2 y3) (- y2 y3))) 1.0d0))
;;;                             (sqrt (float (+ (* (- x3 x1) (- x3 x1)) (* (- y3 y1) (- y3 y1))) 1.0d0)))))
;;;                   (setf c0-sum (+ c0-sum p))))))))
;;;       ;; Each triangle involving (5,0) is found twice by the loop (for the other 2 vertices).
;;;       ;; Adding it once more ensures it's counted 3 times total.
;;;       (setf total-sum (+ total-sum (/ c0-sum 2.0d0))))
;;;     
;;;     ;; Each triangle is counted 3 times (once for each vertex as P3).
;;;     (format nil "~,4F" (/ total-sum 3.0d0))))

;;; ;; Execution
;;; ;; (solve-project-euler-0264)


;;; #+| Do it | (solve-project-euler-0264 )

;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0264 (:use cl))
(in-package #:project-euler-0264)

;; ==============================================================================
;; Project Euler 0264: Triangle Centres (SKDT Emergence & Debt Clearance)
;; ------------------------------------------------------------------------------
;; 【二諦随伴プロトコルによる爆縮】
;; ピタゴラス数への還元は分岐の複雑化(世俗的負債)を招くため、
;; 元の幾何学的真理(H=A+B+C)を維持したまま、以下の「中道の現成」を行う。
;; 1. 対称性の利用: y3 >= 0 のみを探索し、空間を半減(O(R^2/2))。
;; 2. Bitwise Debt Clearance: m2 が平方数かどうかの判定に 63, 64, 65 を
;;    法とするビットマスクフィルターを導入し、重い isqrt 呼び出しを激減させる。
;; ==============================================================================

;; 平方剰余の高速判定用ビットマスク（コンパイル時に計算され定数化される）
(defconstant +sq-mask-64+
  (let ((m 0)) (loop for i from 0 to 63 do (setf m (logior m (ash 1 (mod (* i i) 64))))) m))
(defconstant +sq-mask-63+
  (let ((m 0)) (loop for i from 0 to 62 do (setf m (logior m (ash 1 (mod (* i i) 63))))) m))
(defconstant +sq-mask-65+
  (let ((m 0)) (loop for i from 0 to 64 do (setf m (logior m (ash 1 (mod (* i i) 65))))) m))

(declaim (inline is-sq-candidate-p))
(defun is-sq-candidate-p (n)
  "nが平方数である可能性をO(1)のビット演算で判定する。"
  (declare (type (signed-byte 64) n)
           (optimize (speed 3) (safety 0)))
  (and (logbitp (logand n 63) +sq-mask-64+)
       (logbitp (mod n 63) +sq-mask-63+)
       (logbitp (mod n 65) +sq-mask-65+)))

(defun solve-project-euler-0264 ()
  (let ((total-sum 0.0d0)
        (limit 100000.0d0)
        ;; Rの上限は余裕を持たせても、フィルターの恩恵で一瞬で終わる
        (max-r 50000))
    (declare (optimize (speed 3) (safety 0) (debug 0))
             (type double-float total-sum limit)
             (type fixnum max-r))
    
    (loop for x3 from (- max-r) to max-r do
      ;; 空間の削ぎ落とし：x3^2 + y3^2 <= max-r^2 を満たす y3 のみ探索
      (let ((max-y3 (isqrt (- (* max-r max-r) (* x3 x3)))))
        (declare (type fixnum max-y3))
        ;; 対称性の利用：上半面のみ探索
        (loop for y3 from 0 to max-y3 do
          (let* ((x3-5 (- x3 5))
                 (c (+ (* x3-5 x3-5) (* y3 y3))))
            (declare (type fixnum x3-5 c))
            (when (> c 0)
              (let* ((k (+ (* x3 x3) (* y3 y3)))
                     (m2 (* (the (signed-byte 64) c)
                            (the (signed-byte 64) (- (* 4 k) c)))))
                (declare (type (signed-byte 64) k m2))
                
                ;; 爆縮: ビットマスクによる O(1) ガードレール
                (when (and (>= m2 0) (is-sq-candidate-p m2))
                  (let ((m (isqrt m2)))
                    (declare (type (signed-byte 64) m))
                    (when (= (* m m) m2)
                      (let* ((2c (* 2 c))
                             (num1-x (+ (* (- 5 x3) c) (* y3 m)))
                             (num1-y (+ (* (- y3) c) (* (- 5 x3) m))))
                        (when (and (zerop (mod num1-x 2c))
                                   (zerop (mod num1-y 2c)))
                          (let* ((x1 (truncate num1-x 2c))
                                 (y1 (truncate num1-y 2c))
                                 (x2 (- 5 x3 x1))
                                 (y2 (- 0 y3 y1)))
                            (unless (or (and (= x1 x3) (= y1 y3))
                                        (and (= x1 x2) (= y1 y2))
                                        (and (= x2 x3) (= y2 y3)))
                              (let ((p (+ (sqrt (float (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))) 1.0d0))
                                          (sqrt (float (+ (* (- x2 x3) (- x2 x3)) (* (- y2 y3) (- y2 y3))) 1.0d0))
                                          (sqrt (float (+ (* (- x3 x1) (- x3 x1)) (* (- y3 y1) (- y3 y1))) 1.0d0)))))
                                (when (<= p limit)
                                  ;; y3 > 0 の場合は対称面(下半面)が存在するため2倍カウント
                                  (let ((multiplier (if (zerop y3) 1.0d0 2.0d0)))
                                    (setf total-sum (+ total-sum (* p multiplier)))))))))))))))))))
    
    ;; Singularity (c=0) の処理 (H(5,0) が頂点の1つの場合)
    (let ((c0-sum 0.0d0))
      (let ((pts '((5 0) (-5 0) (0 5) (0 -5) (3 4) (3 -4) (-3 4) (-3 -4) (4 3) (4 -3) (-4 3) (-4 -3))))
        (dolist (p1 pts)
          (let ((x1 (car p1)) (y1 (cadr p1))
                (x3 5) (y3 0))
            (let ((x2 (- 0 x1)) (y2 (- 0 y1)))
              (unless (or (and (= x1 x3) (= y1 y3))
                          (and (= x1 x2) (= y1 y2))
                          (and (= x2 x3) (= y2 y3)))
                (let ((p (+ (sqrt (float (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))) 1.0d0))
                            (sqrt (float (+ (* (- x2 x3) (- x2 x3)) (* (- y2 y3) (- y2 y3))) 1.0d0))
                            (sqrt (float (+ (* (- x3 x1) (- x3 x1)) (* (- y3 y1) (- y3 y1))) 1.0d0)))))
                  (setf c0-sum (+ c0-sum p))))))))
      (setf total-sum (+ total-sum (/ c0-sum 2.0d0))))
    
    ;; P3としてのカウント重複(3回)を補正
    (format nil "~,4F" (/ total-sum 3.0d0))))

#+| Do it | (solve-project-euler-0264)

;2816417.1055
;(print (time (solve-project-euler-0264)))


:ok :gemini-pro-3.1