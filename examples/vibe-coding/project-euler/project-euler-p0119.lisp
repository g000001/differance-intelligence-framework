;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0119 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0119)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)


(defun sum-digits (n)
  "正整数 n の各桁の和を計算する。"
  (declare (type integer n))
  (iterate (for curr initially n then (floor curr 10))
           (while (plusp curr))
           (sum (mod curr 10))))

(defun solve ()
  "x = S(x)^k を満たす数列 a_n の第30項を求める。"
  (let ((candidates '()))
    ;; 数論的ショートカット: x から S(x) を求めるのではなく、
    ;; 桁の和 s と指数 k を動かして x = s^k を生成する。
    ;; s (桁の和) の上限を 400, k (指数) の上限を 100 とすれば、
    ;; 探索空間は 40,000 通りであり、a_30 を含むのに十分すぎる広さである。
    (iterate (for s from 2 to 400)
             (iterate (for k from 2 to 100)
                      (let ((x (expt s k)))
                        ;; 条件: x の桁の和が s 自身に等しい。
                        ;; かつ、問題文より x は2桁以上 (x >= 10)。
                        (when (and (>= x 10)
                                   (= (sum-digits x) s))
                          (push x candidates)))))
    
    ;; 重複を除去（数学的に x = s^k かつ s = S(x) ならば s は一意だが念のため）
    ;; し、昇順にソートする。
    (let ((sorted (sort (remove-duplicates candidates) #'<)))
      ;; 中間ログ: 問題文の例 (a2, a10) と照らし合わせて検証
      (format t "--- 探索完了 ---~%")
      (format t "見つかった候補数: ~D~%" (length sorted))
      (when (>= (length sorted) 2)
        (format t "検証 a(2)  : ~D (期待値: 512)~%" (nth 1 sorted)))
      (when (>= (length sorted) 10)
        (format t "検証 a(10) : ~D (期待値: 614656)~%" (nth 9 sorted)))
      
      (if (>= (length sorted) 30)
          (let ((result (nth 29 sorted)))
            (format t "最終結果 a(30) = ~D~%" result)
            result)
          (error "十分な数の項が見つかりませんでした。探索範囲 (s, k) を広げてください。")))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
--- 探索完了 ---
見つかった候補数: 38
検証 a(2)  : 512 (期待値: 512)
検証 a(10) : 614656 (期待値: 614656)
最終結果 a(30) = 248155780267521

User time    =        0.302
System time  =        0.012
Elapsed time =        0.312
Allocation   = 189406840 bytes
1203 Page faults
GC time      =        0.008
 |------------------------------------------------------------|#
;;→ 248155780267521
:ok
