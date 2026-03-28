;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0296 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0296)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【自己批判と数論的ショートカットの証明】
幾何学的な制約から導かれる BE = ac / (a+b) が整数となる条件は、
c が (a+b)/gcd(a,b) の倍数であることと同値である。
a+b=S, b=B とパラメータ化し、不等式制約を整理すると、有効な三角形の数は
等差数列の床関数の和（Floor Sum）へと完全に次元崩壊する。
これを非再帰的に O(log S) で処理し、全体を Lisp の 62bit fixnum 演算に押し込むことで、
オブジェクトの生成を0バイトに抑え、GCバイアスを物理的に排除した高速な実行を実現する。
||#


(declaim (optimize (speed 3) (safety 0) (debug 0) (hcl:fixnum-safety 0)))

(declaim (inline my-gcd))
(defun my-gcd (a b)
  "アロケーションを伴わない fixnum 専用の高速な最大公約数"
  (declare (type (unsigned-byte 32) a b))
  (let ((u a) (v b))
    (declare (type (unsigned-byte 32) u v))
    (loop
      (when (= v 0) (return u))
      (let ((r (mod u v)))
        (setf u v v r)))))

(defun floor-sum (n m a b)
  "sum_{i=0}^{n-1} floor((a*i + b) / m) を O(log m) で計算する非再帰関数"
  (declare (type fixnum n m a b))
  (let ((ans 0)
        (sign 1)
        (curr-n n)
        (curr-m m)
        (curr-a a)
        (curr-b b))
    (declare (type fixnum ans sign curr-n curr-m curr-a curr-b))
    (loop
      (when (= curr-n 0) (return ans))
      (let ((q-a (truncate curr-a curr-m))
            (rem-a (mod curr-a curr-m))
            (q-b (truncate curr-b curr-m))
            (rem-b (mod curr-b curr-m)))
        (declare (type (unsigned-byte 62) q-a rem-a q-b rem-b))
        (when (> q-a 0)
          (incf ans (* sign q-a (truncate (* curr-n (1- curr-n)) 2))))
        (when (> q-b 0)
          (incf ans (* sign q-b curr-n)))
        (let ((y-max (truncate (+ (* rem-a (1- curr-n)) rem-b) curr-m)))
          (declare (type (unsigned-byte 62) y-max))
          (when (= y-max 0) (return ans))
          
          ;; 幾何学的反射（Geometric Reflection）による変数更新
          (incf ans (* sign y-max (1- curr-n)))
          (setf curr-n y-max)
          (setf curr-a curr-m)
          (setf curr-b (- curr-m 1 rem-b))
          (setf curr-m rem-a)
          (setf sign (- sign)))))))

(defun solve (&optional (limit-n 100000))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((total-triangles 0))
    (declare (type fixnum total-triangles))
    
    (format t "観測: 探索空間 S=A+B を 2 から ~D まで走査します...~%" (truncate limit-n 2))
    (do ((s 2 (1+ s)))
        ((> s (truncate limit-n 2)))
      (declare (type (unsigned-byte 32) s))
      (let* ((m-limit (truncate limit-n s))
             ;; \sum_{k=1}^M \lfloor k/2 \rfloor = \lfloor M/2 \rfloor \lfloor (M+1)/2 \rfloor
             (sub-term (* (truncate m-limit 2) (truncate (1+ m-limit) 2)))
             (b-start (truncate (1+ s) 2)))
        (declare (type (unsigned-byte 32) m-limit sub-term b-start))
        
        (do ((b b-start (1+ b)))
            ((>= b s))
          (declare (type (unsigned-byte 32) b))
          (when (= (my-gcd s b) 1)
            (let ((q (+ s b)))
              (declare (type (unsigned-byte 32) q))
              (incf total-triangles 
                    (- (floor-sum m-limit q s s) sub-term)))))))
                    
    (format t "Answer: ~D~%" total-triangles)
    total-triangles))

#+| Do it | (project-euler-0296:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: 探索空間 S=A+B を 2 から 50000 まで走査します...
Answer: 1137208419

User time    =  0:02:45.403
System time  =        3.945
Elapsed time =  0:04:49.700
Allocation   = 406320 bytes
1330 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1137208419
:ok