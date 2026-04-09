;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0082 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0082)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)


(defun load-matrix-from-file (filepath matrix-size)
  "指定されたテキストファイルからカンマ区切りの行列データを読み込む"
  (let ((matrix (make-array (list matrix-size matrix-size) :element-type 'fixnum)))
    (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
      (unless stream
        (error "File not found: ~A" filepath))
      (iterate
        (for row-index from 0 below matrix-size)
        (for line = (read-line stream nil nil))
        (while line)
        (let ((start-pos 0))
          (iterate
            (for col-index from 0 below matrix-size)
            (for comma-pos = (position #\, line :start start-pos))
            (setf (aref matrix row-index col-index) 
                  (parse-integer line :start start-pos :end comma-pos))
            (when comma-pos 
              (setf start-pos (1+ comma-pos)))))))
    matrix))

(defun solve-dp (matrix matrix-size)
  "1次元配列による動的計画法で最小経路和を計算する"
  (let ((dp-state (make-array matrix-size :element-type 'fixnum)))
    
    ;; 1. 初期状態: 最初の列(列0)の値をセット
    (iterate 
      (for row-index from 0 below matrix-size)
      (setf (aref dp-state row-index) (aref matrix row-index 0)))

    ;; 2. 列1から最終列まで状態を前進させる
    (iterate 
      (for col-index from 1 below matrix-size)
      
      ;; Pass 1 (Right): 直前の列からそのまま右に移動した場合のコストを加算
      (iterate 
        (for row-index from 0 below matrix-size)
        (incf (aref dp-state row-index) (aref matrix row-index col-index)))
      
      ;; Pass 2 (Down): 上から下への移動を評価
      (iterate 
        (for row-index from 1 below matrix-size)
        (setf (aref dp-state row-index)
              (min (aref dp-state row-index)
                   (+ (aref dp-state (1- row-index)) 
                      (aref matrix row-index col-index)))))
      
      ;; Pass 3 (Up): 下から上への移動を評価 (逆順)
      (iterate 
        (for row-index from (- matrix-size 2) downto 0)
        (setf (aref dp-state row-index)
              (min (aref dp-state row-index)
                   (+ (aref dp-state (1+ row-index)) 
                      (aref matrix row-index col-index)))))
      
      ;; デバッグログ (10列ごとに進行状況と現在の列の最小値をトレース)
      (when (zerop (mod col-index 10))
        (let ((current-col-min (iterate (for val in-vector dp-state) (minimize val))))
          (format t "Trace [Col ~2D]: Current minimal path cost is ~D~%" col-index current-col-min))))

    ;; 3. 最終列のうち、最小コストのものを抽出
    (iterate 
      (for final-val in-vector dp-state)
      (minimize final-val))))

(defun solve (&optional (filepath "/tmp/0082_matrix.txt") (matrix-size 80))
  "Project Euler 0082: Minimal path sum"
  (format t "Loading matrix data from ~A...~%" filepath)
  (handler-case
      (let* ((matrix-data (load-matrix-from-file filepath matrix-size))
             (minimal-path (solve-dp matrix-data matrix-size)))
        (format t "Algorithm finished.~%")
        (format t "Minimum Path Sum: ~D~%" minimal-path)
        minimal-path)
    (error (condition)
      (format t "Error: ~A~%" condition)
      (format t "Please ensure 'matrix.txt' is in the working directory.~%"))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Loading matrix data from /tmp/0082_matrix.txt...
Trace [Col 10]: Current minimal path cost is 34599
Trace [Col 20]: Current minimal path cost is 63243
Trace [Col 30]: Current minimal path cost is 93957
Trace [Col 40]: Current minimal path cost is 128231
Trace [Col 50]: Current minimal path cost is 157931
Trace [Col 60]: Current minimal path cost is 194159
Trace [Col 70]: Current minimal path cost is 227254
Algorithm finished.
Minimum Path Sum: 260324

User time    =        0.003
System time  =        0.000
Elapsed time =        0.002
Allocation   = 285984 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 260324
:ok