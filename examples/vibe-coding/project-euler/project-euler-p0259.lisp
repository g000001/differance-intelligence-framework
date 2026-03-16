;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0259 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0259)

(defvar *memo-table* (make-hash-table :test 'equal))

(defun get-digits-as-int (start end)
  "1から9の数字の範囲 [start, end] を連結した整数を返す"
  (let ((val 0))
    (iterate (for d from start to end)
      (setf val (+ (* val 10) d)))
    val))

(defun compute-reachable (start end)
  "範囲 [start, end] で構成可能な有理数の集合を返す"
  (let ((key (cons start end)))
    (multiple-value-bind (cached-val found) (gethash key *memo-table*)
      (if found
          cached-val
          (let ((results (make-hash-table :test 'eql)))
            ;; 1. 連結による数値そのもの
            (setf (gethash (get-digits-as-int start end) results) t)
            ;; 2. 分割して四則演算
            (iterate (for mid from start below end)
              (let ((left-set (compute-reachable start mid))
                    (right-set (compute-reachable (1+ mid) end)))
                (iterate (for (v1 _) in-hashtable left-set)
                  (iterate (for (v2 _) in-hashtable right-set)
                    ;; 加算
                    (setf (gethash (+ v1 v2) results) t)
                    ;; 減算
                    (setf (gethash (- v1 v2) results) t)
                    ;; 乗算
                    (setf (gethash (* v1 v2) results) t)
                    ;; 除算 (0除算回避)
                    (unless (zerop v2)
                      (setf (gethash (/ v1 v2) results) t))))))
            (setf (gethash key *memo-table*) results))))))

(defun solve ()
  (clrhash *memo-table*)
  (let ((final-set (compute-reachable 1 9))
        (positive-integers (make-hash-table :test 'eql))
        (sum 0))
    (iterate (for (val _) in-hashtable final-set)
      (when (and (plusp val) (integerp val))
        (setf (gethash val positive-integers) t)))
    (iterate (for (int _) in-hashtable positive-integers)
      (setf sum (+ sum int)))
    sum))

;; 自己分析
;; 1. 計算量削減の制約:
;;    数字の順序が「1から9までこの順で」と固定されていることが最大の制約です。
;;    これにより、可能な分割パターンが区間DPの範疇に収まり、状態数が O(N^2)、
;;    遷移が O(N)（集合の直積を除く）となり、爆発を防いでいます。
;; 2. 現実的な時間での終了:
;;    各区間で作れる有理数の集合サイズが懸念されますが、Common Lispの
;;    ハッシュテーブルと有理数型を用いることで、重複を効率よく排除しており、
;;    数秒以内に終了すると予測されます。無限ループの危険性は低い（再帰が常に
;;    区間を小さくしているため）です。
;; 3. LLMが陥りやすい罠:
;;    「浮動小数点数による誤差」が最大の罠です。除算が含まれるため、単純な
;;    double等の使用は、整数判定において誤答を招きます。有理数型(rational)を
;;    第一級オブジェクトとして扱うLispの特性がこの問題に対する「勝義諦」となります。
;;    また、単項マイナスが禁止されている点も、実装に含めないよう注意が必要です。


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =  0:01:49.515
System time  =        1.723
Elapsed time =  0:02:06.164
Allocation   = 584736920 bytes
186248 Page faults
GC time      =        1.582
 |------------------------------------------------------------|#
;;→ 20101196798
:ok