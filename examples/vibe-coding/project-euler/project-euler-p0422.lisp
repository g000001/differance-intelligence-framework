#|;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0422 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0422)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defun next-hyperbola-point (px-prev2 py-prev2 px-prev1 py-prev1)
  "P_{i-2} と P_{i-1} から P_i を計算する。
   P_i P_{i-1} は P_{i-2} X(7, 1) と平行であり、双曲線 H 上にある。"
  (let* ((x-point-X 7)
         (y-point-X 1)
         (delta-x (- px-prev2 x-point-X))
         (delta-y (- py-prev2 y-point-X)))
    (if (zerop delta-x)
        ;; 直線が垂直 (x = px-prev1) の場合
        ;; 双曲線の式 12x^2 + 7xy - 12y^2 = 625 に代入
        ;; 12y^2 - 7(px-prev1)y + (625 - 12(px-prev1)^2) = 0
        ;; 解と係数の関係より、y_1 + y_2 = 7*x / 12
        (let* ((next-x px-prev1)
               (sum-y (/ (* 7 px-prev1) 12))
               (next-y (- sum-y py-prev1)))
          (values next-x next-y))
        ;; 直線が傾き m を持つ場合
        (let* ((slope-m (/ delta-y delta-x))
               (intercept-c (- py-prev1 (* slope-m px-prev1)))
               ;; y = mx + c を 12x^2 + 7xy - 12y^2 = 625 に代入
               ;; x^2(12 + 7m - 12m^2) + x(7c - 24mc) - 12c^2 - 625 = 0
               (coef-a (- (+ 12 (* 7 slope-m)) (* 12 slope-m slope-m)))
               (coef-b (- (* 7 intercept-c) (* 24 slope-m intercept-c)))
               ;; 解と係数の関係より、x_1 + x_2 = -B / A
               (sum-x (/ (- coef-b) coef-a))
               (next-x (- sum-x px-prev1))
               (next-y (+ (* slope-m next-x) intercept-c)))
          (values next-x next-y)))))

(defun format-answer-mod (x-coord y-coord)
  "座標 (a/b, c/d) から (a + b + c + d) mod 1,000,000,007 を計算する"
  (let ((num-a (numerator x-coord))
        (num-b (denominator x-coord))
        (num-c (numerator y-coord))
        (num-d (denominator y-coord)))
    (mod (+ num-a num-b num-c num-d) 1000000007)))

(defun solve ()
  (format t "Step 1: Common Lispの無限精度有理数による P_n の軌跡シミュレーション~%")
  (let ((point-x-1 13) (point-y-1 61/4)
        (point-x-2 -43/6) (point-y-2 -4)
        (points-hash (make-hash-table)))
    
    (setf (gethash 1 points-hash) (cons point-x-1 point-y-1))
    (setf (gethash 2 points-hash) (cons point-x-2 point-y-2))
    
    (iterate (for curr-idx from 3 to 12)
             (let* ((pt-prev2 (gethash (- curr-idx 2) points-hash))
                    (pt-prev1 (gethash (- curr-idx 1) points-hash)))
               (multiple-value-bind (new-x new-y)
                   (next-hyperbola-point (car pt-prev2) (cdr pt-prev2)
                                         (car pt-prev1) (cdr pt-prev1))
                 (setf (gethash curr-idx points-hash) (cons new-x new-y)))))
                 
    (format t "~%[観測データ]~%")
    (iterate (for curr-idx from 1 to 10)
             (let ((pt (gethash curr-idx points-hash)))
               (format t "P_~A = (~A, ~A)~%" curr-idx (car pt) (cdr pt))))
               
    (let* ((pt-7 (gethash 7 points-hash))
           (ans-7 (format-answer-mod (car pt-7) (cdr pt-7))))
      (format t "~%[境界値検証]~%")
      (format t "P_7 座標: (~A, ~A)~%" (car pt-7) (cdr pt-7))
      (format t "P_7 モジュロ計算値: ~A (期待値: 806236837) -> ~A~%" 
              ans-7 (if (= ans-7 806236837) "一致!" "不一致!")))
              
    (format t "~%※※※※※ 現実的なコードではありません ※※※※※~%")
    (format t "これは N = 11^14 には到達できないシミュレーションコードです。~%")
    (format t "次のフェーズ (Maximaによる母関数・漸化式の抽出) の準備が整いました。~%")
    0))

Step 1: Common Lispの無限精度有理数による P_n の軌跡シミュレーション

[観測データ]
P_1 = (13, 61/4)
P_2 = (-43/6, -4)
P_3 = (-19/2, -229/24)
P_4 = (1267/144, -37/12)
P_5 = (-4339/288, -63349/3456)
P_6 = (-16954363/497664, 1028893/41472)
P_7 = (17194218091/143327232, 274748766781/1719926784)
P_8 = (-1152929130204331963/855945643032576, 72056746749318493/71328803586048)
P_9 = (-4951760193614517476767283299/122680319758319203909632, -79228162186007370199006872709/1472163837099830446915584)
P_10 = (22835963083296192482100906591725694039452641267/1260092222195718836233500990239234064384, -1427247692705867171595138071612409374457336037/105007685182976569686125082519936172032)

[境界値検証]
P_7 座標: (17194218091/143327232, 274748766781/1719926784)
P_7 モジュロ計算値: 806236837 (期待値: 806236837) -> 一致!

|#

#|

  factor(12*x^2 + 7*x*y - 12*y^2);
/* 出力: (4*x - 3*y) * (3*x + 4*y) */

(%i1) factor(12*x^2 + 7*x*y - 12*y^2);
(%o1)                      - (3 y - 4 x) (4 y + 3 x)

|#

;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0422 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0422)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defconstant $mod-val 1000000007)
(defconstant $mod-exp 1000000006) ; フェルマーの小定理による指数の法 (P - 1)

(defun mod-add (num-a num-b mod-m)
  (declare (type (unsigned-byte 62) num-a num-b mod-m))
  (let ((sum (+ num-a num-b)))
    (if (>= sum mod-m) (- sum mod-m) sum)))

(defun mod-sub (num-a num-b mod-m)
  (declare (type (unsigned-byte 62) num-a num-b mod-m))
  (let ((diff (- num-a num-b)))
    (if (< diff 0) (+ diff mod-m) diff)))

(defun mod-mul (num-a num-b mod-m)
  (declare (type (unsigned-byte 62) num-a num-b mod-m))
  (mod (* num-a num-b) mod-m))

(defun power (base-val exp-val mod-m)
  (declare (type (unsigned-byte 62) base-val exp-val mod-m))
  (let ((res 1)
        (curr-base (mod base-val mod-m))
        (curr-exp exp-val))
    (iterate (while (> curr-exp 0))
             (when (oddp curr-exp)
               (setf res (mod-mul res curr-base mod-m)))
             (setf curr-base (mod-mul curr-base curr-base mod-m))
             (setf curr-exp (ash curr-exp -1)))
    res))

(defun mat-mul (matrix-a matrix-b mod-m)
  (list (mod-add (mod-mul (first matrix-a) (first matrix-b) mod-m)
                 (mod-mul (second matrix-a) (third matrix-b) mod-m) mod-m)
        (mod-add (mod-mul (first matrix-a) (second matrix-b) mod-m)
                 (mod-mul (second matrix-a) (fourth matrix-b) mod-m) mod-m)
        (mod-add (mod-mul (third matrix-a) (first matrix-b) mod-m)
                 (mod-mul (fourth matrix-a) (third matrix-b) mod-m) mod-m)
        (mod-add (mod-mul (third matrix-a) (second matrix-b) mod-m)
                 (mod-mul (fourth matrix-a) (fourth matrix-b) mod-m) mod-m)))

(defun mat-pow (matrix-base exp-val mod-m)
  (let ((res-mat '(1 0 0 1))
        (curr-mat matrix-base)
        (curr-exp exp-val))
    (iterate (while (> curr-exp 0))
             (when (oddp curr-exp)
               (setf res-mat (mat-mul res-mat curr-mat mod-m)))
             (setf curr-mat (mat-mul curr-mat curr-mat mod-m))
             (setf curr-exp (ash curr-exp -1)))
    res-mat))

(defun solve ()
  (let* (($limit-n #.(expt 11 14))
         ($inv-3 (power 3 (- $mod-val 2) $mod-val))
         ($inv-4 (power 4 (- $mod-val 2) $mod-val)))
    
    (format t "Step 1: O(log N) フィボナッチ行列累乗を実行中...~%")
    ;; 行列 M = [[1, 1], [1, 0]] の N-1 乗を計算し、F_N, F_{N-1}, F_{N-2} を mod 10^9+6 で取得
    (let* (($matrix-pow (mat-pow '(1 1 1 0) (1- $limit-n) $mod-exp))
           ($fib-n     (first $matrix-pow))
           ($fib-n-1   (second $matrix-pow))
           ($fib-n-2   (fourth $matrix-pow))
           ;; K2 = F_N + F_{N-2}
           ($pow-k2    (mod-add $fib-n $fib-n-2 $mod-exp)))
      
      (format t "Step 2: 閉形式有理数の各項をモジュラ冪乗で復元中...~%")
      ;; 必要な冪乗項を計算
      (let* ((pow3-2fn1 (power 3 (mod-mul 2 $fib-n-1 $mod-exp) $mod-val))
             (pow2-2k2  (power 2 (mod-mul 2 $pow-k2 $mod-exp) $mod-val))
             (pow3-fn1  (power 3 $fib-n-1 $mod-val))
             (pow2-k2   (power 2 $pow-k2 $mod-val))
             
             ;; 各フラクションの項 a, b, c, d を計算
             ;; a = 3^{2F_{N-1}-1} + 2^{2K_2-2}
             (num-a (mod-add (mod-mul pow3-2fn1 $inv-3 $mod-val)
                             (mod-mul pow2-2k2 $inv-4 $mod-val)
                             $mod-val))
             ;; b = 2^{K_2-2} * 3^{F_{N-1}-1}
             (num-b (mod-mul (mod-mul pow2-k2 $inv-4 $mod-val)
                             (mod-mul pow3-fn1 $inv-3 $mod-val)
                             $mod-val))
             ;; c = 2^{2K_2+2} - 3^{2F_{N-1}+1}
             (num-c (mod-sub (mod-mul pow2-2k2 4 $mod-val)
                             (mod-mul pow3-2fn1 3 $mod-val)
                             $mod-val))
             ;; d = 2^{K_2} * 3^{F_{N-1}}
             (num-d (mod-mul pow2-k2 pow3-fn1 $mod-val)))
        
        (format t "Step 3: 最終モジュラ加算を実行中...~%")
        (let ((final-ans (mod (+ num-a num-b num-c num-d) $mod-val)))
          (format t "Final Result P(~A): ~A~%" $limit-n final-ans)
          final-ans)))))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: O(log N) フィボナッチ行列累乗を実行中...
Step 2: 閉形式有理数の各項をモジュラ冪乗で復元中...
Step 3: 最終モジュラ加算を実行中...
Final Result P(379749833583241): 92060460

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 6368 bytes
0 Page faults
Calls to %EVAL    114
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 92060460
:ok

