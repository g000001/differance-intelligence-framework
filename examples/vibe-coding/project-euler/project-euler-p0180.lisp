;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
;; iterateライブラリが利用可能であることを前提とします
;; (ql:quickload :iterate) 
(defpackage #:project-euler-0180 (:use cl #:iterate))
(in-package #:project-euler-0180)

#||
(cl-text https://purl.org/aletheics/ontologies/tta/euler180.clif

  (cl-comment "=== 1. ACX Jump: Algebraic Reduction of f_n(x,y,z) (勝義的還元) ===")
  (cl-comment "
    高次多項式 f_n の愚直な探索は、非中道の誤謬（NMF）を引き起こす。
    f_n(x,y,z) を因数分解することで、次のように勝義諦へ跳躍する:
    f_n(x,y,z) = (x + y + z)(x^n + y^n - z^n)
    x, y, z は正の有理数ゆえ、x + y + z > 0。
    したがって、f_n(x,y,z) = 0 ⇔ x^n + y^n = z^n。
  ")
  (forall (x y z n)
    (if (and (PositiveRational x) (PositiveRational y) (PositiveRational z))
        (iff (= (f_n x y z) 0)
             (= (+ (expt x n) (expt y n)) (expt z n)))))

  (cl-comment "=== 2. Mathematical Constraint: Fermat's Last Theorem (公理的定礎) ===")
  (cl-comment "
    有理数における x^n + y^n = z^n の解は、フェルマーの最終定理より
    |n| >= 3 において存在しない。また、n = 0 は 1+1=1 となり不適。
    よって探索すべき次元 n は {1, 2, -1, -2} のみに限定（圧縮）される。
  ")
  (forall (x y z n)
    (if (and (= (+ (expt x n) (expt y n)) (expt z n))
             (PositiveRational x) (PositiveRational y) (PositiveRational z))
        (member n (set 1 2 -1 -2))))

  (cl-comment "=== 3. Exact Integer Projection (浮動小数点の排除と中道の現成) ===")
  (cl-comment "
    z を求める際、浮動小数点の sqrt による丸め誤差（世俗の幻影）を排し、
    Lisp の有理数型と isqrt による厳密な平方根判定 (sqrt-rational) によって
    完全な全単射空間を構築する。
  ")
  (forall (q)
    (if (Rational q)
        (iff (PerfectSquare q)
             (and (PerfectSquareInteger (numerator q))
                  (PerfectSquareInteger (denominator q))))))

)
||#



(defun sqrt-rational (q)
  "有理数 q の平方根が有理数になる場合、その値を返す。ならない場合は NIL。"
  (let* ((num (numerator q))
         (den (denominator q))
         (snum (isqrt num))
         (sden (isqrt den)))
    (when (and (= (* snum snum) num)
               (= (* sden sden) den))
      (/ snum sden))))

(defun valid-z-p (z k)
  "z が 0 < z < 1 であり、かつ分母が k 以下の有理数であるかを判定する。"
  (and z
       (rationalp z)
       (< 0 z 1)
       (<= (denominator z) k)))

(defun solve-180 (&optional (k 35))
  "Project Euler 180 を解く。"
  (let ((f-list nil)
        (sums '()))
    
    ;; 1. 次数 k 以下の Farey 数列の生成（互いに素な分数の列挙）
    (iterate (for b from 2 to k)
      (iterate (for a from 1 to (1- b))
        (when (= (gcd a b) 1)
          (push (/ a b) f-list))))
    (setf f-list (sort f-list #'<))
    
    ;; 2. 探索空間の巡回と条件の検証
    (iterate (for tail on f-list)
      (for x = (car tail))
      (iterate (for y in tail)
        ;; ACX Jump: n ∈ {1, 2, -1, -2} に対する z の計算
        ;; n=1  => z = x + y
        ;; n=2  => z = sqrt(x^2 + y^2)
        ;; n=-1 => z = (1/x + 1/y)^(-1) = (x*y)/(x+y)
        ;; n=-2 => z = (1/x^2 + 1/y^2)^(-1/2) = sqrt( (x^2 * y^2) / (x^2 + y^2) )
        (let* ((x2 (* x x))
               (y2 (* y y))
               (z-candidates
                 (list (+ x y)
                       (sqrt-rational (+ x2 y2))
                       (/ (* x y) (+ x y))
                       (sqrt-rational (/ (* x2 y2) (+ x2 y2))))))
          
          ;; 有効な z であれば、s(x,y,z) をリストに追加 (重複は pushnew で排除)
          (iterate (for z in z-candidates)
            (when (valid-z-p z k)
              (pushnew (+ x y z) sums))))))
    
    ;; 3. 全ての distinct な s(x,y,z) の和 t = u/v を求め、u+v を返す
    (let* ((total-sum (reduce #'+ sums))
           (u (numerator total-sum))
           (v (denominator total-sum)))
      (+ u v))))

;; 実行: (solve-180 35)
#+| Do it | (solve-180 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-180)

User time    =        0.377
System time  =        0.017
Elapsed time =        0.304
Allocation   = 27722672 bytes
1610 Page faults
GC time      =        0.001
 |------------------------------------------------------------|#
;;→ 285196020571078987
:ok
