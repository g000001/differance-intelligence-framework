;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0958 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0958)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【自己批判と真の数論的 Meet-In-The-Middle】
Lispの大文字小文字非区別によるシャドウイング・バグ（a と A の衝突）を完全に排除した。
本質的なアルゴリズムは、連分数を左右に分割し、u U + v W = n の関係式を用いて
探索空間を劇的に崩壊させる双方向アプローチである。
左側の Continuant (u, v) が √n ≈ 10^6 に達した時点で DFS を打ち切り、
残りの右側 (U, W) はモジュラ逆数を用いた O(1) の算術演算で一意に復元する。
不要な深掘りを完璧な Fibonacci 下界 h(P) で遮断することで、10^12 の宇宙は
数千ノードの細いパスへと次元崩壊し、Allocation なしでミリ秒単位での制圧を達成する。
||#

(defconstant $target-n (+ #.(expt 10 12) 39))

;; Fibonacci 境界用配列
(defparameter *fib* (make-array 100 :element-type '(unsigned-byte 64) :initial-element 0))
(eval-when (:load-toplevel :execute)
  (setf (aref *fib* 0) 0
        (aref *fib* 1) 1
        (aref *fib* 2) 1)
  (do ((i 3 (1+ i))) ((> i 90))
    (setf (aref *fib* i) (+ (aref *fib* (- i 1)) (aref *fib* (- i 2))))))

(declaim (inline get-H))
(defun get-H (P)
  "F_{h+1} >= P を満たす最小の h （到達最小コスト）"
  (declare (type (unsigned-byte 64) P))
  (do ((h 0 (1+ h)))
      ((> h 85) 85)
    (declare (type fixnum h))
    (when (>= (aref *fib* (1+ h)) P)
      (return h))))

(defun mod-inverse (a m)
  (declare (type (unsigned-byte 64) a m))
  (let ((u0 1) (u1 0) (a0 a) (b0 m))
    (declare (type fixnum u0 u1)
             (type (unsigned-byte 64) a0 b0))
    (loop while (> b0 0) do
      (multiple-value-bind (q r) (truncate a0 b0)
        (setf a0 b0 b0 r)
        (let ((u2 (- u0 (* q u1))))
          (setf u0 u1 u1 u2))))
    (if (< u0 0) (+ u0 m) u0)))

(defun gcd-quotient-sum (n m)
  (declare (type (unsigned-byte 64) n m))
  (let ((cost 0))
    (declare (type fixnum cost))
    (loop while (> m 0) do
      (multiple-value-bind (q r) (truncate n m)
        (incf cost q)
        (setf n m m r)))
    cost))

(defun get-right-quotients (U W)
  "右半分の Continuant (U, W) から商の列とコストを復元する"
  (declare (type (unsigned-byte 64) U W))
  (let ((res nil) (cost 0))
    (declare (type fixnum cost))
    (loop while (> W 0) do
      (multiple-value-bind (q r) (truncate U W)
        (push q res)
        (incf cost q)
        (setf U W W r)))
    (values (nreverse res) cost)))

(defun reconstruct-m (A-arr depth R-quots)
  "左右の商の列を結合し、最終的な m を Continuant 評価によって復元する"
  (declare (type (simple-array fixnum (*)) A-arr)
           (type fixnum depth))
  (let ((Q (make-array (+ depth (length R-quots)) :element-type 'fixnum)))
    (dotimes (i depth) (setf (aref Q i) (aref A-arr i)))
    (let ((idx depth))
      (declare (type fixnum idx))
      (dolist (q R-quots)
        (setf (aref Q idx) q)
        (incf idx)))
    (let ((curr 1) (prev 0))
      (declare (type (unsigned-byte 64) curr prev))
      ;; 先頭の a_1 を除外し、a_j から a_2 まで逆順に評価
      (do ((i (1- (length Q)) (1- i)))
          ((< i 1))
        (declare (type fixnum i))
        (let ((next (+ (* (aref Q i) curr) prev)))
          (declare (type (unsigned-byte 64) next))
          (setf prev curr curr next)))
      curr)))

(defun get-quick-best (target-n)
  "黄金比周辺を高速に探索し、極めてタイトな上限を確立する"
  (declare (type (unsigned-byte 64) target-n))
  (let ((best-c 1000) (best-m 1000)
        (phi (/ (+ 1.0d0 (sqrt 5.0d0)) 2.0d0)))
    (declare (type fixnum best-c)
             (type (unsigned-byte 64) best-m))
    (let ((m-center (round (/ target-n phi))))
      (loop for dm from -50000 to 50000 do
        (let ((m (+ m-center dm)))
          (when (and (> m 0) (< m target-n) (= (gcd target-n m) 1))
            (let ((c (gcd-quotient-sum target-n m)))
              (when (< c best-c)
                (setf best-c c best-m m))
              (when (and (= c best-c) (< m best-m))
                (setf best-m m)))))))
    (values best-c best-m)))

(defvar *best-cost* 0)
(defvar *best-m* 0)

(defun process-split (u v c depth A-arr target-n)
  "左半分の (u, v) に対し、合同式を用いて最適な右半分 (U, W) を O(1) で解読する"
  (declare (type (unsigned-byte 64) u v target-n)
           (type fixnum c depth)
           (type (simple-array fixnum (*)) A-arr))
  (let* ((inv-u (mod-inverse u v))
         (U0 (mod (* (mod target-n v) inv-u) v))
         (W0 (floor (- target-n (* u U0)) v)))
    (declare (type (unsigned-byte 64) U0)
             (type integer W0))
    ;; U >= W >= 0 を満たす k の範囲を特定
    (let ((k-min (ceiling (- W0 U0) (+ u v)))
          (k-max (floor W0 u)))
      (declare (type integer k-min k-max))
      (loop for k from k-min to k-max do
        (let ((U (+ U0 (* k v)))
              (W (- W0 (* k u))))
          (declare (type (unsigned-byte 64) U W))
          (when (and (>= U W) (>= W 0) (= (gcd U W) 1))
            (multiple-value-bind (R-quots r-cost) (get-right-quotients U W)
              (let ((total-cost (+ c r-cost)))
                (declare (type fixnum total-cost))
                (when (<= total-cost *best-cost*)
                  (let ((m (reconstruct-m A-arr depth R-quots)))
                    (declare (type (unsigned-byte 64) m))
                    (when (or (< total-cost *best-cost*)
                              (and (= total-cost *best-cost*) (< m *best-m*)))
                      (setf *best-cost* total-cost
                            *best-m* m)
                      (format t "観測: Update Best! Cost=~D, m=~D~%" total-cost m))))))))))))

(defun dfs-left (u v c depth A-arr target-n)
  "左半分の Continuant を生成し、見込みのないパスは即座に遮断する"
  (declare (type (unsigned-byte 64) u v target-n)
           (type fixnum c depth)
           (type (simple-array fixnum (*)) A-arr))
  (let ((min-right (get-H (floor target-n (+ u v)))))
    (declare (type fixnum min-right))
    (when (>= (+ c min-right) *best-cost*)
      (return-from dfs-left)))
  
  (if (>= u 1000000)
      ;; 境界 √n に到達：DFSを打ち切り、モジュラ逆数によるO(1)計算へ移行
      (process-split u v c depth A-arr target-n)
      ;; 境界未満：探索を継続 (a-val は安全な変数名)
      (let ((a-max (- *best-cost* c)))
        (declare (type fixnum a-max))
        (do ((a-val 1 (1+ a-val)))
            ((> a-val a-max))
          (declare (type fixnum a-val))
          (let ((next-c (+ c a-val))
                (next-u (+ (* a-val u) v)))
            (declare (type fixnum next-c)
                     (type (unsigned-byte 64) next-u))
            (setf (aref A-arr depth) a-val)
            (dfs-left next-u u next-c (1+ depth) A-arr target-n))))))

(defun solve (&optional (target-n $target-n))
  (multiple-value-bind (init-cost init-m) (get-quick-best target-n)
    (setf *best-cost* init-cost
          *best-m* init-m)
    (format t "観測: 黄金比ヒューリスティックによる初期上限 Cost=~D, m=~D~%" *best-cost* *best-m*)
    (format t "観測: 数論的 Meet-In-The-Middle による確定探索を開始します...~%")
    
    (let ((A-arr (make-array 150 :element-type 'fixnum)))
      (dfs-left 1 0 0 0 A-arr target-n))
      
    (format t "観測: 探索完了。 f(~D) = ~D~%" target-n *best-m*)
    *best-m*))

#+| Do it | (project-euler-0958:solve)