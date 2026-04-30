;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0113 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0113)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defun ncr (n r)
  "二項係数 nCr を計算する。Common Lispのbignumにより精度は保証される。"
  (if (or (< r 0) (> r n))
      0
      (let ((r (min r (- n r)))
            (result 1))
        (iterate (for i from 1 to r)
                 (setf result (/ (* result (+ n 1 (- i))) i)))
        result)))

(defun count-non-bouncy (limit-digits)
  "10^limit-digits 未満の非弾む数(non-bouncy numbers)の総数を計算する。"
  (let* ((n limit-digits)
         ;; 増加数の総数: (n+9)C9 - 1
         ;; 00...0 (全て0) を除外するため -1
         (increasing (1- (ncr (+ n 9) 9)))
         ;; 減少数の総数: Σ_{k=1}^{n} ( (k+9)C9 - 1 ) = (n+10)C10 - 1 - n
         (decreasing (- (ncr (+ n 10) 10) 1 n))
         ;; 増加数かつ減少数（レピュニット/単一数字）: 9 * n
         (both (* 9 n))
         ;; 包除原理: |Inc ∪ Dec| = |Inc| + |Dec| - |Inc ∩ Dec|
         (total (- (+ increasing decreasing) both)))
    
    ;; 中間ログの出力
    (format t "Digits: ~D~%" n)
    (format t "Increasing: ~D~%" increasing)
    (format t "Decreasing: ~D~%" decreasing)
    (format t "Both (Repdigits): ~D~%" both)
    
    total))

(defun solve ()
  "Project Euler P113 を解く。10^100 未満の非弾む数を求める。"
  (let ((googol-digits 100))
    (format t "--- Starting Project Euler P113 ---~%")
    ;; 問題文の例での自己検算
    (assert (= (count-non-bouncy 6) 12951))
    (assert (= (count-non-bouncy 10) 277032))
    
    ;; 本番計算
    (let ((result (count-non-bouncy googol-digits)))
      (format t "Final Result (below 10^100): ~A~%" result)
      result)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
--- Starting Project Euler P113 ---
Digits: 6
Increasing: 5004
Decreasing: 8001
Both (Repdigits): 54
Digits: 10
Increasing: 92377
Decreasing: 184745
Both (Repdigits): 90
Digits: 100
Increasing: 4263421511270
Decreasing: 46897636623880
Both (Repdigits): 900
Final Result (below 10^100): 51161058134250

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 1408 bytes
1 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 51161058134250
:ok