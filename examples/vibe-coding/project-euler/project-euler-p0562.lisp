;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
;;; externalImage: https://projecteuler.net/problem=562
(cl:in-package cl-user)
(defpackage #:project-euler-0562 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0562)

(defun compute-ext-gcd (a b)
  (prog ((s0 1) (s1 0) (t0 0) (t1 1) (r0 a) (r1 b) q-quot r-rem s-next t-next)
   loop
    (if (= r1 0) (return (values r0 s0 t0)))
    (setf q-quot (floor r0 r1))
    (setf r-rem (- r0 (* q-quot r1)))
    (setf s-next (- s0 (* q-quot s1)))
    (setf t-next (- t0 (* q-quot t1)))
    (setf r0 r1 r1 r-rem)
    (setf s0 s1 s1 s-next)
    (setf t0 t1 t1 t-next)
    (go loop)))

(defun solve ()
  (let* (($r 10000000)
         ($2r (* 2 $r))
         ($4r2 (* 4 $r $r))
         (max-L 0.0d0)
         (best-R/r 0.0d0))
    (prog ((q $2r) p-max p g s t-val ux uy v2 c a b L R/r)
     
     loop-q
      ;; 弦の長さ c が 2r に近いものから順に調べる。
      ;; 周長 L は 2c 程度になるため、2c が既存の max-L を下回ったら終了。
      (if (<= (* 2 q) max-L) (go end))
      
      (setf p-max (isqrt (- $4r2 (* q q))))
      (setf p p-max)
      
     loop-p
      (if (< p 1) (go next-q))
      (setf v2 (+ (* q q) (* p p)))
      (setf c (sqrt (float v2 0d0)))
      
      ;; 枝刈り: この (q, p) での理論上の最大周長が既存記録を越えられないなら p ループを抜ける
      (if (<= (* 2.0d0 c) max-L) (go next-q))
      
      ;; 原始的な格子点 (gcd=1) のみが面積 1/2 の三角形の底辺になり得る
      (multiple-value-setq (g s t-val) (compute-ext-gcd q p))
      (if (/= g 1) (go next-p))
      
      ;; 三角形の高さは 1/c。極めて平べったいため、
      ;; 弦の両端が円周に接する付近に第3の点 A が存在すれば最大。
      ;; 拡張ユークリッド互除法から得られる解を元に、円の境界付近をピンポイントで確認
      (setf ux (- t-val))
      (setf uy s)
      
      ;; 周長 L の簡易計算 (平坦な三角形では L ≒ 2c + (1/c) 程度)
      ;; ここでは厳密な頂点 A の円内判定を簡略化し、境界上の chord を評価
      (setf a (sqrt (float (+ (* ux ux) (* uy uy)) 0d0)))
      (setf b (sqrt (float (+ (* (- q ux) (- q ux)) (* (- p uy) (- p uy))) 0d0)))
      (setf L (+ c a b))
      
      (if (> L max-L)
          (progn
            (setf max-L L)
            ;; T(r) = R/r = (abc/2) / r = abc / (2r)
            (setf R/r (/ (* c a b) (* 2.0d0 $r)))
            (format t "Debug: New Best T(r) approx: ~F (q=~A, p=~A)~%" R/r q p)))
      
     next-p
      (decf p)
      (go loop-p)
      
     next-q
      (decf q)
      (go loop-q)
      
     end)
    (let ((ans (round best-R/r)))
      (format t "T(~A) = ~A~%" $r ans)
      ans)))

#+| Do it | (project-euler-0562:solve)