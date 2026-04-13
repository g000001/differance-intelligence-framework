;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0982 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0982)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)



(defun get-rolls ()
  "3つのサイコロの出目の重複組合せ(56通り)を昇順リストとして生成"
  (let ((res nil))
    (iterate (for i from 1 to 6)
      (iterate (for j from i to 6)
        (iterate (for k from j to 6)
          (push (list i j k) res))))
    (nreverse res)))

(defun get-revealed ()
  "公開される2つのサイコロの出目の重複組合せ(21通り)を昇順リストとして生成"
  (let ((res nil))
    (iterate (for i from 1 to 6)
      (iterate (for j from i to 6)
        (push (list i j) res)))
    (nreverse res)))

(defun ways (roll)
  "出目の組合せが216通りの中で何通り出現するかを計算 (対称性の活用)"
  (destructuring-bind (a b c) roll
    (cond ((and (= a b) (= b c)) 1)
          ((or (= a b) (= b c)) 3)
          (t 6))))

(defun get-hidden (v r)
  "ロール r の中から ペア v を公開した際に隠されるサイコロの目を返す. v が r の部分多重集合でない場合は nil"
  (let ((v1 (first v)) (v2 (second v))
        (r1 (first r)) (r2 (second r)) (r3 (third r)))
    (cond
      ((and (= v1 r1) (= v2 r2)) r3)
      ((and (= v1 r1) (= v2 r3)) r2)
      ((and (= v1 r2) (= v2 r3)) r1)
      (t nil))))

(defun solve-simplex (M N A)
  "有理数(Rational)演算による厳密なシンプレックス法 (1フェーズ)"
  (let ((pivot-count 0))
    (iterate
      (incf pivot-count)
      ;; 中間ログの出力 (10回ごとにZの推移を観測)
      (when (zerop (mod pivot-count 10))
        (format t "[DEBUG] Simplex Pivot ~A, Current Exact Expected Value (Unscaled) = ~A~%" 
                pivot-count (aref A M (+ N M))))

      ;; ピボット列の探索 (最小の負の還元費用)
      (let ((min-val 0)
            (pivot-col -1))
        (iterate (for j from 0 below (+ N M))
          (when (< (aref A M j) min-val)
            (setf min-val (aref A M j))
            (setf pivot-col j)))
        
        ;; 最適解到達判定
        (when (>= min-val 0)
          (format t "[DEBUG] Optimal solution reached in ~A iterations.~%" pivot-count)
          (return (aref A M (+ N M))))

        ;; ピボット行の探索 (Blandの規則に準拠した最小比テスト)
        (let ((min-ratio nil)
              (pivot-row -1))
          (iterate (for i from 0 below M)
            (let ((val (aref A i pivot-col)))
              (when (> val 0)
                (let ((ratio (/ (aref A i (+ N M)) val)))
                  (when (or (null min-ratio) (< ratio min-ratio))
                    (setf min-ratio ratio)
                    (setf pivot-row i))))))

          (when (null min-ratio)
            (error "Linear Program is Unbounded!"))

          ;; 掃き出し演算 (ピボット操作)
          (let ((pivot-val (aref A pivot-row pivot-col)))
            (iterate (for j from 0 to (+ N M))
              (setf (aref A pivot-row j) (/ (aref A pivot-row j) pivot-val)))
            (iterate (for i from 0 to M)
              (unless (= i pivot-row)
                (let ((factor (aref A i pivot-col)))
                  (unless (zerop factor)
                    (iterate (for j from 0 to (+ N M))
                      (decf (aref A i j) (* factor (aref A pivot-row j))))))))))))))

(defun solve ()
  "問題 P982 のメインプロトコル"
  (let* ((rolls (get-rolls))
         (revealed (get-revealed))
         ;; 変数の数 N = 56 (V_r) + 21 (q_v) = 77
         (N (+ 56 21))
         ;; 制約の数 M = 126 (妥当な r,v ペア) + 21 (q_v上限) = 147
         (M (+ 126 21))
         ;; シンプレックスタブロー (全て0の有理数で初期化)
         (A (make-array (list (1+ M) (1+ (+ N M))) :initial-element 0))
         (row 0))
    
    (format t "[DEBUG] Building LP Tableau: Variables=~A, Constraints=~A~%" N M)

    ;; 1. 制約: V_r - (\max(v) - h) q_v <= h
    (iterate (for r in rolls)
             (for r-idx from 0)
      (iterate (for v in revealed)
               (for v-idx from 0)
        (let ((h (get-hidden v r)))
          (when h
            (setf (aref A row r-idx) 1)
            (setf (aref A row (+ 56 v-idx)) (- h (max (first v) (second v))))
            (setf (aref A row (+ N row)) 1) ; スラック変数
            (setf (aref A row (+ N M)) h)   ; 右辺値 (RHS)
            (incf row)))))
    
    ;; 2. 制約: q_v <= 1
    (iterate (for v in revealed)
             (for v-idx from 0)
      (setf (aref A row (+ 56 v-idx)) 1)
      (setf (aref A row (+ N row)) 1) ; スラック変数
      (setf (aref A row (+ N M)) 1)   ; 右辺値 (RHS)
      (incf row))
    
    ;; 目的関数: Maximize \sum ways(r) * V_r
    ;; (タブローの最下段には符号を反転して格納)
    (iterate (for r in rolls)
             (for r-idx from 0)
      (setf (aref A M r-idx) (- (ways r))))

    ;; シンプレックス法による厳密な有理数解の導出
    (let ((exact-max-expected (solve-simplex M N A)))
      (format t "[DEBUG] Exact rational answer: ~A / ~A~%" 
              (numerator exact-max-expected) 
              (* 216 (denominator exact-max-expected)))
      
      ;; 全事象(216)で割り、指定の6桁の少数点フォーマットで出力
      (format nil "~,6F" (float (/ exact-max-expected 216) 1.0d0)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[DEBUG] Building LP Tableau: Variables=77, Constraints=147
[DEBUG] Simplex Pivot 10, Current Exact Expected Value (Unscaled) = 72
[DEBUG] Simplex Pivot 20, Current Exact Expected Value (Unscaled) = 216
[DEBUG] Simplex Pivot 30, Current Exact Expected Value (Unscaled) = 402
[DEBUG] Simplex Pivot 40, Current Exact Expected Value (Unscaled) = 560
[DEBUG] Simplex Pivot 50, Current Exact Expected Value (Unscaled) = 635
[DEBUG] Simplex Pivot 60, Current Exact Expected Value (Unscaled) = 754
[DEBUG] Simplex Pivot 70, Current Exact Expected Value (Unscaled) = 846
[DEBUG] Simplex Pivot 80, Current Exact Expected Value (Unscaled) = 1871/2
[DEBUG] Optimal solution reached in 82 iterations.
[DEBUG] Exact rational answer: 1893 / 432

User time    =        0.006
System time  =        0.000
Elapsed time =        0.004
Allocation   = 321136 bytes
16 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "4.381944"
:ok