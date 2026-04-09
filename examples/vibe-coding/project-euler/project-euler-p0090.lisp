;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0090 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0090)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)



;;; 平方数のペア (01, 04, 09, 16, 25, 36, 49, 64, 81) のビットマスク表現を事前計算
;;; 6 と 9 は拡張ビットマスク側で吸収されるため、ターゲット側の数字はそのまま扱う
(defparameter *target-masks*
  (let ((targets '((0 1) (0 4) (0 9) (1 6) (2 5) (3 6) (4 9) (6 4) (8 1))))
    (mapcar (lambda (pair)
              (cons (ash 1 (car pair)) (ash 1 (cadr pair))))
            targets)))

(defun combinations (k list)
  "リストから K 個の要素を選ぶすべての組み合わせを生成する再帰関数"
  (cond ((zerop k) '(()))
        ((null list) '())
        (t (nconc (mapcar (lambda (c) (cons (car list) c))
                          (combinations (1- k) (cdr list)))
                  (combinations k (cdr list))))))

(defun extend-cube-to-mask (cube)
  "立方体の数字リストを10-bitのマスクに変換。6と9の同値性（不変量）を適用する。"
  (declare (optimize speed))
  (let ((mask 0))
    (declare (type fixnum mask))
    ;; まず通常のビットを立てる
    (dolist (digit cube)
      (setf mask (logior mask (the fixnum (ash 1 digit)))))
    ;; 6 (ビット6: 64) または 9 (ビット9: 512) があれば、両方を立てて拡張する
    (when (or (logtest mask 64)
              (logtest mask 512))
      (setf mask (logior mask 576))) ;; 576 = 64 + 512
    mask))

(defun valid-pair-p (m1 m2)
  "2つの立方体（のビットマスク）が、すべての平方数を表現可能か検証する"
  (declare (type fixnum m1 m2) (optimize speed))
  (iterate (for target in *target-masks*)
    (let ((a (car target))
          (b (cdr target)))
      (declare (type fixnum a b))
      ;; m1がaを持ちm2がbを持つ、または、m1がbを持ちm2がaを持つ
      (always (or (and (logtest m1 a) (logtest m2 b))
                  (and (logtest m1 b) (logtest m2 a)))))))

(defun solve ()
  "PE0090 エントリーポイント"
  (format t "[*] Project Euler 0090 Solver Initiated.~%")
  ;; 1. 210通りの基礎立方体を生成
  (let* ((cubes (combinations 6 '(0 1 2 3 4 5 6 7 8 9)))
         ;; 2. ビットマスク化による次元の崩壊
         (masks-list (mapcar #'extend-cube-to-mask cubes))
         (masks (coerce masks-list 'simple-vector))
         (n (length masks))
         (valid-count 0))
    (declare (type fixnum n valid-count))
    (format t "[*] Extracted distinct cubes (invariants): ~D~%" n)
    
    ;; 3. 全てのペアの組み合わせを検証（j は i から開始して重複ペアを排除）
    (iterate (for i from 0 below n)
      (let ((m1 (svref masks i)))
        (declare (type fixnum m1))
        (iterate (for j from i below n)
          (let ((m2 (svref masks j)))
            (declare (type fixnum m2))
            (when (valid-pair-p m1 m2)
              (incf valid-count))))))
    
    ;; 結果の出力
    (format t "[*] Total valid distinct arrangements found: ~D~%" valid-count)
    valid-count))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[*] Project Euler 0090 Solver Initiated.
[*] Extracted distinct cubes (invariants): 210
[*] Total valid distinct arrangements found: 1217

User time    =        0.000
System time  =        0.000
Elapsed time =        0.001
Allocation   = 45648 bytes
15 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1217
:ok