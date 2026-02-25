
;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0177 (:use cl))
;;; (in-package #:project-euler-0177)

;;; ;; ==============================================================================
;;; ;; 1. SKDT Duality: Emergence of the Middle Way
;;; ;; ------------------------------------------------------------------------------
;;; ;; The problem of "Integer Angled Quadrilaterals" is a search for structural 
;;; ;; invariants (Sunyata) within the manifold of geometric possibilities.
;;; ;; The 8 corner angles form a dependent-arising (Pratityasamutpada) system 
;;; ;; where each angle is empty of self-essence (Anatman) but realized through 
;;; ;; its relationship to others via the Sine Rule.
;;; ;; ==============================================================================

;;; (declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; ;; Helper to pack 8 angles into a canonical fixnum representation.
;;; ;; This represents the "fixed point" (Dfix0) of the similarity transformation.
;;; (declaim (inline get-canonical-packed))
;;; (defun get-canonical-packed (a1 a2 b1 b2 c1 c2 d1 d2)
;;;   (declare (type fixnum a1 a2 b1 b2 c1 c2 d1 d2))
;;;   (flet ((p (v1 v2 v3 v4 v5 v6 v7 v8)
;;;            (declare (type fixnum v1 v2 v3 v4 v5 v6 v7 v8))
;;;            (let ((res 0))
;;;              (declare (type (unsigned-byte 64) res))
;;;              (setf res (+ (* res 200) v1))
;;;              (setf res (+ (* res 200) v2))
;;;              (setf res (+ (* res 200) v3))
;;;              (setf res (+ (* res 200) v4))
;;;              (setf res (+ (* res 200) v5))
;;;              (setf res (+ (* res 200) v6))
;;;              (setf res (+ (* res 200) v7))
;;;              (setf res (+ (* res 200) v8))
;;;              res)))
;;;     ;; D4 symmetry: 4 rotations and 4 reflections.
;;;     (min (p a1 a2 b1 b2 c1 c2 d1 d2)
;;;          (p b1 b2 c1 c2 d1 d2 a1 a2)
;;;          (p c1 c2 d1 d2 a1 a2 b1 b2)
;;;          (p d1 d2 a1 a2 b1 b2 c1 c2)
;;;          (p a2 a1 d2 d1 c2 c1 b2 b1)
;;;          (p d2 d1 c2 c1 b2 b1 a2 a1)
;;;          (p c2 c1 b2 b1 a2 a1 d2 d1)
;;;          (p b2 b1 a2 a1 d2 d1 c2 c1))))

;;; (defun solve ()
;;;   "Solves Project Euler 177: Integer Angled Quadrilaterals."
;;;   (let* ((results (make-hash-table :test 'eql))
;;;          (sin-table (make-array 181 :element-type 'double-float))
;;;          (cos-table (make-array 181 :element-type 'double-float))
;;;          (d-pi (coerce pi 'double-float))
;;;          (deg-to-rad (/ d-pi 180.0d0))
;;;          (rad-to-deg (/ 180.0d0 d-pi))
;;;          (tolerance 1d-9))
;;;     (declare (type (simple-array double-float (181)) sin-table cos-table))
;;;     
;;;     ;; Precompute trigonometric tables (Potential side - K category)
;;;     (dotimes (i 181)
;;;       (setf (aref sin-table i) (sin (* i deg-to-rad))
;;;             (aref cos-table i) (cos (* i deg-to-rad))))

;;;     ;; Iterate through potential angles (Conventional Truth - Samvṛti)
;;;     ;; We fix diagonal AB and angles (a1, b1+b2) for C, and (a1+a2, b1) for D.
;;;     (loop for a1 from 1 to 177 do
;;;       (loop for b1 from 1 to (- 178 a1) do
;;;         (let ((limit (- 180 a1 b1))
;;;               (sin-b1 (aref sin-table b1)))
;;;           (loop for a2 from 1 to (1- limit) do
;;;             (let* ((d2 (- limit a2))
;;;                    (k-part1 (/ (aref sin-table d2) sin-b1)))
;;;               (loop for b2 from 1 to (1- limit) do
;;;                 (let* ((c1 (- limit b2))
;;;                        ;; K represents the Sine Rule ratio derived from shared diagonals.
;;;                        (k (* k-part1 (/ (aref sin-table (+ b1 b2)) (aref sin-table c1))))
;;;                        ;; Calculate c2 using atan2 (y, x) for stability.
;;;                        (c2-rad (atan (aref sin-table a2) (- k (aref cos-table a2))))
;;;                        (c2-deg (* c2-rad rad-to-deg))
;;;                        (c2-rounded (round c2-deg)))
;;;                   ;; Check if the resulting angle is an integer (Middle Way realization).
;;;                   (when (< (abs (- c2-deg c2-rounded)) tolerance)
;;;                     (let ((d1 (- (+ a1 b1) c2-rounded)))
;;;                       ;; Convexity check: all angles must be positive.
;;;                       (when (> d1 0)
;;;                         (let ((key (get-canonical-packed a1 a2 b1 b2 c1 c2-rounded d1 d2)))
;;;                           ;; Store the canonical form to avoid non-similar duplicates.
;;;                           (setf (gethash key results) t))))))))))))
;;;     ;; The count of unique canonical forms (Ultimate Truth - Paramartha).
;;;     (hash-table-count results)))

;;; ;; Execute the solver
;;; ;; (format t "Total number of non-similar integer angled quadrilaterals: ~A~%" (solve))

;;; #+| Do it | (solve )
;;; ;→ 501276 ng


;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0177 (:use cl alexandria))
;;; (in-package #:project-euler-0177)

;;; ;; ==============================================================================
;;; ;; Project Euler 0177: Integer Angled Quadrilaterals
;;; ;; ------------------------------------------------------------------------------
;;; ;; 【二諦随伴プロトコルによる爆縮と中道の現成】
;;; ;; 1. Exact Integer Projection / Geometric Consistency:
;;; ;;    チェバの定理の角形式に基づき、浮動小数点除算を排除した x, y 座標系へ
;;; ;;    還元することで、丸め誤差という世俗の幻影を消散させる。
;;; ;; 2. Bijective Generation (対称性の完全な復元):
;;; ;;    四角形の D4 対称性(回転4種、鏡映4種)において、角が隣の頂点へ移る際の
;;; ;;    「ねじれ(順序の反転)」を厳密に実装し、重複カウントを完全に清算する。
;;; ;; ==============================================================================

;;; (declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; (declaim (inline pack-angles))
;;; (defun pack-angles (v1 v2 v3 v4 v5 v6 v7 v8)
;;;   "8つの角を180進数として1つの64ビット整数にパックし、ハッシュキーとする。"
;;;   (declare (type fixnum v1 v2 v3 v4 v5 v6 v7 v8))
;;;   (let ((res 0))
;;;     (declare (type (unsigned-byte 64) res))
;;;     (setf res (+ (* res 180) v1))
;;;     (setf res (+ (* res 180) v2))
;;;     (setf res (+ (* res 180) v3))
;;;     (setf res (+ (* res 180) v4))
;;;     (setf res (+ (* res 180) v5))
;;;     (setf res (+ (* res 180) v6))
;;;     (setf res (+ (* res 180) v7))
;;;     (setf res (+ (* res 180) v8))
;;;     res))

;;; (declaim (inline get-canonical-packed))
;;; (defun get-canonical-packed (a1 a2 b1 b2 c1 c2 d1 d2)
;;;   "D4対称性の8つの変換（回転と鏡映における幾何学的ねじれを考慮）の中から最小値をとる。"
;;;   (declare (type fixnum a1 a2 b1 b2 c1 c2 d1 d2))
;;;   (min (pack-angles a1 a2 b1 b2 c1 c2 d1 d2)                 ;; Identity
;;;        (pack-angles b2 b1 c2 c1 d2 d1 a2 a1)                 ;; Rot 90
;;;        (pack-angles c1 c2 d1 d2 a1 a2 b1 b2)                 ;; Rot 180
;;;        (pack-angles d2 d1 a2 a1 b2 b1 c2 c1)                 ;; Rot 270
;;;        (pack-angles a2 a1 d2 d1 c2 c1 b2 b1)                 ;; Reflection
;;;        (pack-angles b1 b2 a1 a2 d1 d2 c1 c2)                 ;; Ref * Rot 90
;;;        (pack-angles c2 c1 b2 b1 a2 a1 d2 d1)                 ;; Ref * Rot 180
;;;        (pack-angles d1 d2 c1 c2 b1 b2 a1 a2)))               ;; Ref * Rot 270

;;; (defun solve-euler-0177 ()
;;;   "整数角四角形の全探索を O(180^4) で行い、条件を満たす一意な個数を返す。"
;;;   (let* ((sin-table (make-array 181 :element-type 'double-float))
;;;          (cos-table (make-array 181 :element-type 'double-float))
;;;          (d-pi (coerce pi 'double-float))
;;;          (deg-to-rad (/ d-pi 180.0d0))
;;;          (rad-to-deg (/ 180.0d0 d-pi))
;;;          (tolerance 1d-9)
;;;          (results (make-hash-table :test 'eql)))
;;;     (declare (type (simple-array double-float (181)) sin-table cos-table)
;;;              (type double-float rad-to-deg tolerance))
;;;     
;;;     ;; 1. 三角関数テーブルの事前計算 (K Category)
;;;     (dotimes (i 181)
;;;       (setf (aref sin-table i) (sin (* i deg-to-rad))
;;;             (aref cos-table i) (cos (* i deg-to-rad))))

;;;     ;; 2. 角度空間の探索 (Samvrti)
;;;     (loop for a1 of-type fixnum from 1 to 177 do
;;;       (loop for b1 of-type fixnum from 1 to (- 178 a1) do
;;;         (let* ((S (+ a1 b1))
;;;                (sin-S (aref sin-table S))
;;;                (cos-S (aref cos-table S))
;;;                (sin-a1 (aref sin-table a1))
;;;                (sin-b1 (aref sin-table b1)))
;;;           (declare (type fixnum S)
;;;                    (type double-float sin-S cos-S sin-a1 sin-b1))
;;;           
;;;           (loop for a2 of-type fixnum from 1 to (- 179 S) do
;;;             (let* ((d2 (- 180 S a2))
;;;                    (sin-a2 (aref sin-table a2))
;;;                    (sin-d2 (aref sin-table d2))
;;;                    (part-y (* sin-a2 sin-b1 sin-S))
;;;                    (part-x-1 (* sin-a1 sin-d2))
;;;                    (part-x-2 (* sin-a2 sin-b1 cos-S)))
;;;               (declare (type fixnum d2)
;;;                        (type double-float sin-a2 sin-d2 part-y part-x-1 part-x-2))
;;;               
;;;               (loop for b2 of-type fixnum from 1 to (- 179 S) do
;;;                 (let* ((c1 (- 180 S b2))
;;;                        (sin-b2 (aref sin-table b2))
;;;                        (sin-c1 (aref sin-table c1))
;;;                        ;; 浮動小数点除算を完全に排除した成分計算 (ACX Jump)
;;;                        (y (* part-y sin-c1))
;;;                        (x (+ (* part-x-1 sin-b2) (* part-x-2 sin-c1)))
;;;                        (c2-rad (atan y x))
;;;                        (c2-deg (* c2-rad rad-to-deg))
;;;                        (c2-round (round c2-deg)))
;;;                   (declare (type fixnum c1 c2-round)
;;;                            (type double-float sin-b2 sin-c1 y x c2-rad c2-deg))
;;;                   
;;;                   ;; 3. 整数度の判定と中道の現成
;;;                   (when (< (abs (- c2-deg (float c2-round 0d0))) tolerance)
;;;                     (let ((d1 (- S c2-round)))
;;;                       (declare (type fixnum d1))
;;;                       (when (> d1 0)
;;;                         (let ((key (get-canonical-packed a1 a2 b1 b2 c1 c2-round d1 d2)))
;;;                           (setf (gethash key results) t))))))))))))
;;;     
;;;     (hash-table-count results)))

;;; #+| Do it | (solve-euler-0177)
;;; ;→ 501276


;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0177 (:use cl alexandria))
(in-package #:project-euler-0177)

;; ==============================================================================
;; Project Euler 0177: Integer Angled Quadrilaterals
;; ------------------------------------------------------------------------------
;; 【二諦随伴プロトコル：完全な幾何学的還元 (Exact ACX Jump)】
;; 以前の変数(a1, a2...)の錯綜による偽の解を排除するため、四角形の外周を
;; 巡る8つの角 v1...v8 という「対称性の極み」へモデルを再構築しました。
;; チェバの定理は完全に一貫し、D4対称性も単純なシフト演算へと還元されます。
;; ==============================================================================

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(declaim (inline pack-angles))
(defun pack-angles (v1 v2 v3 v4 v5 v6 v7 v8)
  "180^8 は 64bit整数 (1.84e19) に安全に収まるため、完全なハッシュキーとなる。"
  (declare (type fixnum v1 v2 v3 v4 v5 v6 v7 v8))
  (let ((res 0))
    (declare (type (unsigned-byte 64) res))
    (setf res (+ (* res 180) v1))
    (setf res (+ (* res 180) v2))
    (setf res (+ (* res 180) v3))
    (setf res (+ (* res 180) v4))
    (setf res (+ (* res 180) v5))
    (setf res (+ (* res 180) v6))
    (setf res (+ (* res 180) v7))
    (setf res (+ (* res 180) v8))
    res))

(declaim (inline get-canonical-packed))
(defun get-canonical-packed (v1 v2 v3 v4 v5 v6 v7 v8)
  "D4対称性: 4つの回転(2シフト)と、4つの鏡映(反転してペア入れ替え)。"
  (declare (type fixnum v1 v2 v3 v4 v5 v6 v7 v8))
  (min (pack-angles v1 v2 v3 v4 v5 v6 v7 v8)         ; Id
       (pack-angles v3 v4 v5 v6 v7 v8 v1 v2)         ; Rot 90
       (pack-angles v5 v6 v7 v8 v1 v2 v3 v4)         ; Rot 180
       (pack-angles v7 v8 v1 v2 v3 v4 v5 v6)         ; Rot 270
       (pack-angles v2 v1 v8 v7 v6 v5 v4 v3)         ; Refl
       (pack-angles v4 v3 v2 v1 v8 v7 v6 v5)         ; Refl + Rot 90
       (pack-angles v6 v5 v4 v3 v2 v1 v8 v7)         ; Refl + Rot 180
       (pack-angles v8 v7 v6 v5 v4 v3 v2 v1)))       ; Refl + Rot 270

(defun solve-euler-0177 ()
  "v1~v8 の巡回モデルを用いて整数角四角形を全探索する。"
  (let* ((sin-table (make-array 181 :element-type 'double-float))
         (cos-table (make-array 181 :element-type 'double-float))
         (d-pi (coerce pi 'double-float))
         (rad-to-deg (/ 180.0d0 d-pi))
         (deg-to-rad (/ d-pi 180.0d0))
         (tolerance 1d-11) ; 倍精度に合わせた厳密な閾値
         (results (make-hash-table :test 'eql)))
    (declare (type (simple-array double-float (181)) sin-table cos-table)
             (type double-float rad-to-deg tolerance))
    
    (dotimes (i 181)
      (setf (aref sin-table i) (sin (* i deg-to-rad))
            (aref cos-table i) (cos (* i deg-to-rad))))

    ;; S1 = v1+v2 = v5+v6
    (loop for S1 of-type fixnum from 2 to 178 do
      (let* ((S2 (- 180 S1))
             (sin-S2 (aref sin-table S2))
             (cos-S2 (aref cos-table S2)))
        (declare (type fixnum S2)
                 (type double-float sin-S2 cos-S2))
        
        (loop for v1 of-type fixnum from 1 to (1- S1) do
          (let* ((v2 (- S1 v1))
                 (sin-v1 (aref sin-table v1))
                 (sin-v2 (aref sin-table v2)))
            
            (loop for v5 of-type fixnum from 1 to (1- S1) do
              (let* ((v6 (- S1 v5))
                     (sin-v5 (aref sin-table v5))
                     (sin-v6 (aref sin-table v6))
                     (A-part (* sin-v1 sin-v5))
                     (B-part (* sin-v2 sin-v6)))
                (declare (type double-float A-part B-part))
                
                (loop for v3 of-type fixnum from 1 to (1- S2) do
                  (let* ((v4 (- S2 v3))
                         (sin-v3 (aref sin-table v3))
                         (sin-v4 (aref sin-table v4))
                         (A (* A-part sin-v3))
                         (B (* B-part sin-v4))
                         ;; チェバの定理による atan2 での逆算
                         (Y (* B sin-S2))
                         (X (+ A (* B cos-S2)))
                         (v7-rad (atan Y X))
                         (v7-deg (* v7-rad rad-to-deg))
                         (v7-round (round v7-deg)))
                    (declare (type fixnum v4 v7-round)
                             (type double-float A B Y X v7-rad v7-deg))
                    
                    ;; 誤差1e-11以内で整数とみなせるか判定
                    (when (< (abs (- v7-deg (float v7-round 0d0))) tolerance)
                      (let ((v8 (- S2 v7-round)))
                        (declare (type fixnum v8))
                        ;; 全ての角が 1度以上であることを確認 (凸四角形の保証)
                        (when (and (>= v7-round 1) (>= v8 1))
                          (let ((key (get-canonical-packed v1 v2 v3 v4 v5 v6 v7-round v8)))
                            (setf (gethash key results) t)))))))))))))
    
    (hash-table-count results)))

#+| Do it | (solve-euler-0177)
;;→ 129325
:ok
