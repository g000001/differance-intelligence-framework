;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0086 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0086)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(defun ways (x vw)
  "x と vw=y+z の値から、1 <= z <= y <= x を満たす (y, z) の組の数を返す"
  (declare (type fixnum x vw))
  (cond
    ((or (<= vw 1) (> vw (the fixnum (* 2 x)))) 0)
    ((<= vw x) (truncate vw 2))
    (t (the fixnum (- x (truncate (the fixnum (1- vw)) 2))))))

(defun solve-with-limit (target-sum max-m)
  "上限 max-m の下でピタゴラス数を生成し、target-sum を超えたら M を返す"
  (declare (type fixnum target-sum max-m))
  ;; delta配列に各Mごとの追加解数をメモライズする
  (let ((delta (make-array (1+ max-m) :element-type 'fixnum :initial-element 0)))
    (iterate (for m from 2 to (1+ (truncate max-m 2)))
      ;; ショートカット: a <= max-m または b <= max-m になる n の境界を算出
      (let* ((u1 (min (1- m) (truncate max-m (* 2 m))))
             (m2 (* m m))
             (l2 (max 1 (if (>= m2 max-m)
                            (let* ((diff (the fixnum (- m2 max-m)))
                                   (rt (isqrt diff)))
                              (if (= (the fixnum (* rt rt)) diff)
                                  rt
                                  (1+ rt)))
                            1))))
        (declare (type fixnum u1 m2 l2))
        (flet ((process (n)
                 (declare (type fixnum n))
                 (when (and (oddp (- m n))
                            (= 1 (gcd m n)))
                   (let ((a (- m2 (the fixnum (* n n))))
                         (b (* 2 m n)))
                     (declare (type fixnum a b))
                     (iterate (for k from 1)
                       (let ((x1 (* k a))
                             (vw1 (* k b))
                             (x2 (* k b))
                             (vw2 (* k a)))
                         (declare (type fixnum x1 vw1 x2 vw2))
                         ;; aとbの両方の定数倍が上限を超えたら、このm, nの組は終了
                         (when (and (> x1 max-m) (> x2 max-m))
                           (finish))
                         ;; 各ケースについて解を加算
                         (when (<= x1 max-m)
                           (incf (aref delta x1) (ways x1 vw1)))
                         (when (<= x2 max-m)
                           (incf (aref delta x2) (ways x2 vw2)))))))))
          ;; 無駄な領域をスキップし、有効な2つの領域のみを走査する
          (if (< u1 l2)
              (progn
                (iterate (for n from 1 to u1) (process n))
                (iterate (for n from l2 to (1- m)) (process n)))
              (iterate (for n from 1 to (1- m)) (process n))))))
    
    ;; 累積和を計算し、目標値を超えた最初の M を探索
    (let ((sum 0))
      (declare (type fixnum sum))
      (iterate (for x from 1 to max-m)
        (incf sum (aref delta x))
        (when (> sum target-sum)
          (leave x))))))

(defun solve (&optional (target #.(expt 10 6)))
  "指定された target を超える最小の M を探索する"
  (format t "Target threshold: ~D~%" target)
  (iterate (for max-m first 2500 then (* max-m 2))
    (format t "Trying max-M limit = ~A~%" max-m)
    (let ((res (solve-with-limit target max-m)))
      (when res
        (format t "Found solution! M = ~A~%" res)
        (leave res)))))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Target threshold: 1000000
Trying max-M limit = 2500
Found solution! M = 1818

User time    =        0.001
System time  =        0.000
Elapsed time =        0.000
Allocation   = 20328 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1818
:ok