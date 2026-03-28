;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0608 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0608)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【自己批判と数論的ショートカットの証明】
m の約数と k のループによる二重和は、ディリクレ畳み込みを適用することで
「200!の素因数のみから成る無平方数」に対する単一和へと完全に次元崩壊する。
探索空間は N=10^12 から数十万ノードの DFS へと圧縮される。
さらに、累積和 S(x) を毎回 O(√x) で計算するとレッドライン（1分ルール）に抵触する恐れがあるため、
x <= 10^6 の領域を 4MB の固定長配列に O(N log N) で事前計算しておく。
これにより、DFS の膨大な末端ノードの評価が O(1) に崩壊し、Allocation ゼロで Lisp の限界速度を引き出す。
||#

(defconstant +mod+ 1000000007)
(defconstant +max-s+ 1000000)

;; 事前計算された S(x) のテーブル
(defparameter *s-table* (make-array (1+ +max-s+) :element-type '(unsigned-byte 32) :initial-element 0))

(defun build-s-table ()
  "S(x) = Σ_{k=1}^x floor(x/k) の O(1) ルックアップテーブルを構築"
  (declare (optimize (speed 3) (safety 0)))
  (let ((sigma (make-array (1+ +max-s+) :element-type '(unsigned-byte 32) :initial-element 0)))
    (loop for i from 1 to +max-s+ do
      (loop for j from i to +max-s+ by i do
        (incf (aref sigma j))))
    (let ((sum 0))
      (declare (type (unsigned-byte 64) sum))
      (loop for i from 1 to +max-s+ do
        (setf sum (mod (+ sum (aref sigma i)) +mod+))
        (setf (aref *s-table* i) sum)))))

(eval-when (:load-toplevel :execute)
  (build-s-table))

(declaim (inline get-S))
(defun get-S (x)
  "x <= 10^6 なら O(1)、それ以上ならディリクレ双曲線法で O(√x) で S(x) mod 10^9+7 を返す"
  (declare (type (unsigned-byte 64) x)
           (optimize (speed 3) (safety 0)))
  (if (<= x +max-s+)
      (aref *s-table* x)
      (let ((sum 0)
            (sq (isqrt x)))
        (declare (type (unsigned-byte 64) sum sq))
        (loop for i from 1 to sq do
          (setf sum (mod (+ sum (truncate x i)) +mod+)))
        (setf sum (mod (* 2 sum) +mod+))
        (let ((sq-mod (mod sq +mod+)))
          (mod (+ sum +mod+ (- (mod (* sq-mod sq-mod) +mod+))) +mod+)))))

(defun mod-inverse (a m)
  (declare (type (unsigned-byte 64) a m)
           (optimize (speed 3) (safety 0)))
  (let ((u0 1) (u1 0) (a0 a) (b0 m))
    (declare (type fixnum u0 u1)
             (type (unsigned-byte 64) a0 b0))
    (loop while (> b0 0) do
      (multiple-value-bind (q r) (truncate a0 b0)
        (setf a0 b0 b0 r)
        (let ((u2 (- u0 (* q u1))))
          (setf u0 u1 u1 u2))))
    (if (< u0 0) (+ u0 m) u0)))

(defun get-e (p m)
  "m! に含まれる素数 p の指数 e_p をルジャンドルの定理で求める"
  (declare (type (unsigned-byte 32) p m)
           (optimize (speed 3) (safety 0)))
  (let ((count 0) (curr p))
    (loop while (<= curr m) do
      (incf count (truncate m curr))
      (setf curr (* curr p)))
    count))

(defparameter *primes* #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199))

;; グローバルな DFS 探索状態 (アロケーションゼロ化)
(defparameter *active-primes* (make-array 46 :element-type '(unsigned-byte 32) :initial-element 0))
(defparameter *v-prime* (make-array 46 :element-type '(unsigned-byte 64) :initial-element 0))
(defvar *primes-count* 0)

(defun dfs (idx current-c current-v target-n)
  "無平方数を生成しながら D(m,n) の構成要素を加算する"
  (declare (type fixnum idx)
           (type (unsigned-byte 64) current-c current-v target-n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((ans (mod (* current-v (get-S (truncate target-n current-c))) +mod+)))
    (declare (type (unsigned-byte 64) ans))
    (loop for i from idx below *primes-count* do
      (let* ((p (aref *active-primes* i))
             (next-c (* current-c p)))
        (declare (type (unsigned-byte 64) p next-c))
        (if (> next-c target-n)
            (return) ; 素数は昇順なのでここで枝刈り可能
            (let ((next-v (mod (* current-v (aref *v-prime* i)) +mod+)))
              (declare (type (unsigned-byte 64) next-v))
              (setf ans (mod (+ ans (dfs (1+ i) next-c next-v target-n)) +mod+))))))
    ans))

(defun compute-D (m target-n)
  (declare (optimize (speed 3) (safety 0)))
  (setf *primes-count* (count-if (lambda (p) (<= p m)) *primes*))
  (let ((v0 1))
    (declare (type (unsigned-byte 64) v0))
    (loop for i from 0 below *primes-count* do
      (let* ((p (aref *primes* i))
             (e (get-e p m)))
        (setf (aref *active-primes* i) p)
        
        ;; V0 = Π (e+1)(e+2)/2 の計算
        (let* ((t1 (mod (1+ e) +mod+))
               (t2 (mod (+ e 2) +mod+))
               (t-prod (mod (* t1 t2) +mod+))
               (t-div2 (mod (* t-prod (mod-inverse 2 +mod+)) +mod+)))
          (setf v0 (mod (* v0 t-div2) +mod+)))
          
        ;; v'(p) = -e / (e+2) の計算
        (let* ((num (mod (- e) +mod+))
               (den (mod (+ e 2) +mod+))
               (inv (mod-inverse den +mod+)))
          (setf (aref *v-prime* i) (mod (* num inv) +mod+)))))
          
    (let ((dfs-sum (dfs 0 1 1 target-n)))
      (mod (* v0 dfs-sum) +mod+))))

(defun solve ()
  (declare (optimize (speed 3) (safety 0)))
  (format t "観測: D(3!, 10^2) = ~D (Expected: 3398)~%" (compute-D 3 100))
  (format t "観測: D(4!, 10^6) = ~D (Expected: 268882292)~%" (compute-D 4 1000000))
  
  (format t "観測: 本探索 D(200!, 10^12) を開始します...~%")
  (let ((ans (compute-D 200 #.(expt 10 12))))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0608:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: D(3!, 10^2) = 3398 (Expected: 3398)
観測: D(4!, 10^6) = 268882292 (Expected: 268882292)
観測: 本探索 D(200!, 10^12) を開始します...
Answer: 439689828

User time    =        7.484
System time  =        0.067
Elapsed time =        7.493
Allocation   = 274768 bytes
3748 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 439689828
:ok