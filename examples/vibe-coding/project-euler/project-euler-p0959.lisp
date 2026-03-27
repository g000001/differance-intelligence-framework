;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0959 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0959)

(defun newton-root (a b k)
  "方程式 z^{a+b} - 2z^a + 1 = 0 の単位円内の k 番目の根をニュートン法で求める"
  (let* ((a-df (coerce a 'double-float))
         (b-df (coerce b 'double-float))
         ;; z^a = 1/2 の根を初期値とする（真の根への極めて精度の高い近似）
         (theta (/ (* 2.0d0 pi k) a-df))
         (r (expt 0.5d0 (/ 1.0d0 a-df)))
         (z (complex (* r (cos theta)) (* r (sin theta)))))
    (iterate (for iter from 0 to 1000)
      (let* ((za (expt z a))
             (zb (expt z b))
             (zab (* za zb))
             (pz (+ zab (- (* 2.0d0 za)) 1.0d0))
             (pz-prime (* (expt z (1- a)) (+ (* (+ a-df b-df) zb) (- (* 2.0d0 a-df))))))
        ;; 15桁の精度 (double-floatの限界) で収束判定
        (when (< (abs pz) 1.0d-14)
          (return z))
        (let ((dz (/ pz pz-prime)))
          (setf z (- z dz))
          (when (< (abs dz) 1.0d-14)
            (return z)))))
    z))

(defun solve (&optional (a 89) (b 97))
  (format t "Starting Dimensional Collapse (Complex Roots & Residue Theorem) for f(~A, ~A)...~%" a b)
  (let ((c #c(0.0d0 0.0d0))
        (ab (coerce (+ a b) 'double-float))
        (2b (coerce (* 2 b) 'double-float)))
    
    ;; a個の単位円内の根すべてについて留数を合算
    (iterate (for k from 0 below a)
      (let* ((z (newton-root a b k))
             (za (expt z a))
             (num (* 2.0d0 za))
             (den (- ab (* 2b za))))
        (incf c (/ num den))))
    
    (let* ((real-c (realpart c))
           (ans (/ 1.0d0 real-c)))
      (format t "Finished. Answer: ~,9f~%" ans)
      ;; 小数点以下9桁でフォーマットした文字列を返す
      (format nil "~,9f" ans))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Dimensional Collapse (Complex Roots & Residue Theorem) for f(89, 97)...
Finished. Answer: 0.857162085

User time    =        0.007
System time  =        0.000
Elapsed time =        0.004
Allocation   = 3179208 bytes
106 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "0.857162085"
