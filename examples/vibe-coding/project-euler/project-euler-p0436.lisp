;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0436 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0436)

#||
(cl:comment "Project Euler 436: Unfair wager")
(cl:comment "By applying renewal theory to uniform distributions, the game is analytically modeled.")
(cl:comment "Louise's overshoot 'u' and last number 'x' have a joint density of e^{1+u-x} on 0 < u < x < 1.")
(cl:comment "Julie needs to reach a threshold of 1-u. Her process includes a point mass at 0 (crossing on the first draw) and a continuous renewal density e^{s'} for subsequent draws.")
(cl:comment "Integrating the exact condition y > x over the entire domain yields a perfect closed-form analytical solution.")
(cl:comment "P = (1 + 14e - 5e^2) / 4")
(cl:comment "This achieves O(1) time and space complexity, completely bypassing any need for simulation.")
||#

(defun solve ()
  (let* ((e (exp 1d0))
         (e2 (* e e))
         ;; 解析的に導出された完全な閉形式の勝率
         (ans (/ (+ 1d0 (* 14d0 e) (* -5d0 e2)) 4d0)))
    
    ;; 10桁の小数点でフォーマットして返す ("0.abcdefghij" の形式)
    (format nil "~,10F" ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 320 bytes
1 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "0.5276662759"
:ok