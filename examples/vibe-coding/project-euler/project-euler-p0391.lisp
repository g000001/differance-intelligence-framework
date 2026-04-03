;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0391 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0391)

#||
【自己批判と真の次元崩壊】
不正解の原因は、逆算の開始点として「二進数表現がデタラメな中途半端な点」を選んだことで、
フラクタル構造の位相がずれ、真の終端から来る軌道とは異なる「偽のアトラクタ」に合流したことにある。
真の終端 2^(n+1)-1 のフラクタル同型性を維持するためには、
開始点も必ず「すべてのビットが1の数 (2^m - 1)」でなければならない。
状態空間を 1.35億 まで拡張し、k = 2^26-1 と 2^27-1 を絶対同型点として逆算を開始することで、
1000という深い n においても、位相ズレのない真の M(n) 軌道に確定的に合流させる。
||#

(defvar *MAX-K* 135000000)
(defvar *S-array* nil)

(defun precompute-tables ()
  "S_k の配列を線形時間で構築する (約540MB)"
  (setf *S-array* (make-array (1+ *MAX-K*) :element-type '(unsigned-byte 32)))
  (setf (aref *S-array* 0) 0)
  (iterate (for k from 1 to *MAX-K*)
    (setf (aref *S-array* k) (+ (aref *S-array* (1- k)) (logcount k)))))

(defun simulate-orbit (start-k n)
  "二分探索を用いて後手必勝状態を逆算し、0付近の M(n) を求める"
  (let ((V (aref *S-array* start-k))
        (k start-k))
    (loop while (> V n) do
      (let ((target (- V (1+ n))))
        (let ((low 0) (high k))
          (loop while (<= low high) do
            (let ((mid (ash (+ low high) -1)))
              (if (<= (aref *S-array* mid) target)
                  (setf low (1+ mid))
                  (setf high (1- mid)))))
          (setf k (1- low))
          (setf V (aref *S-array* k)))))
    (if (> V 0) V 0)))

(defun get-Mn (n)
  "フラクタル同型点 (2^26-1 と 2^27-1) から真の軌道を計算し M(n) を確定する"
  (let* ((k1 (1- (ash 1 26)))  ; 67,108,863
         (k2 (1- (ash 1 27)))) ; 134,217,727
    (let ((val1 (simulate-orbit k1 n))
          (val2 (simulate-orbit k2 n)))
      (if (= val1 val2)
          val1
          (error "Attractor convergence failed for n=~D (Fractal phase mismatch)" n)))))

(defun solve ()
  (format t "観測: S_k を事前計算中 (MAX_K=~D)...~%" *MAX-K*)
  (precompute-tables)
  
  (format t "観測: テストケース n <= 20 の検証を実行中...~%")
  (let ((sum-20 0))
    (iterate (for n from 1 to 20)
      (incf sum-20 (expt (get-Mn n) 3)))
    (format t "観測: Sum M(n)^3 for n<=20 = ~D (Expected: 8150)~%" sum-20))
    
  (format t "観測: 本探索 Sum M(n)^3 for 1 <= n <= 1000 を実行中...~%")
  (let ((ans 0))
    (iterate (for n from 1 to 1000)
      (incf ans (expt (get-Mn n) 3)))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0391:solve)