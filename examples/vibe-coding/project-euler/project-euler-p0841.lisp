;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: grok (SuperGrok mode)
(cl:in-package cl-user)
(defpackage #:project-euler-0841 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0841)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))
(optimized-code-p nil)

(defun coprime (a b)
  "p > 2q > 0 かつ gcd(p,q)=1 の確認（Bijective Generation遵守）"
  (= (gcd a b) 1))

(defun fib (n)
  "F_n (F_1 = F_2 = 1)"
  (if (<= n 2)
      1
      (iterate (for i from 3 to n)
               (for a from 1)
               (for b from 1)
               (for next = (+ a b))
               (setf a b b next)
               (finally (return b)))))

(defun area-a (p q)
  "A(p, q) — ここが核心。Geminiが導出した閉形式をここに移植してください。
   検証: A(8,3) = 24*(sqrt(2)-1) ≈ 9.9411254970
         A(130021,50008) ≈ 10.9210371479"
  (unless (and (integerp p) (integerp q) (> p (* 2 q)) (> q 0) (coprime p q))
    (error "Invalid {p/q}: p=~D q=~D" p q))

  ;; TODO: Geminiの公式をここに（例: p tan(π/p) の加重和 + kite項のtrig閉形式）
  ;; inradius=1 での R = 1/cos(π q /p) を用いた正確な表現
  (let* ((pi-val (coerce pi 'double-float))
         (raw (coerce (* p q (tan (/ pi-val p))) 'double-float)))  ; placeholder（正しくない）
    
    (format t "~&DEBUG: A(~D, ~D) raw = ~,15F~%" p q raw)
    raw))

(defun solve ()
  "∑_{n=3}^{34} A(F_{n+1}, F_{n-1}) を10桁丸め"
  (let ((total 0.0d0)
        (verified-83 nil))
    (iterate (for n from 3 to 34)
             (for p = (fib (+ n 1)))
             (for q = (fib (- n 1)))
             (for a = (area-a p q))
             (incf total a)
             (when (and (= p 8) (= q 3) (not verified-83))
               (format t "~&Verification: A(8,3) = ~,15F (期待 ≈9.9411254970)~%" a)
               (setf verified-83 t)))
    
    (format t "~&最終合計 (丸め前): ~,15F~%" total)
    (let ((result (format nil "~,10F" total)))
      (format t "~&10桁丸め結果: ~A~%" result)
      result)))

;; (project-euler-0841:solve)

