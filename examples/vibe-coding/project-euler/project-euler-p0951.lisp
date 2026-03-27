;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0951 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0951)

(defun choose (n k)
  "二項係数を計算する（Lispの優秀なRatioにより自動的に精度が保たれる）"
  (let ((res 1))
    (iterate (for i from 1 to k)
      (setf res (/ (* res (- (1+ n) i)) i)))
    res))

(defun solve (&optional (n 26))
  (format t "Starting DP for N=~A (Calculating partitions without block size 2)...~%" n)
  
  ;; dp[i][j]: 合計 i を j 個の「2以外の正整数」に分割する組み合わせの数
  (let ((dp (make-array (list (1+ n) (1+ n)) :element-type 'fixnum :initial-element 0)))
    (setf (aref dp 0 0) 1)
    
    (iterate (for i from 1 to n)
      (iterate (for j from 1 to i)
        (iterate (for x from 1 to i)
          ;; ブロックサイズが2の場合は許容しない
          (when (and (/= x 2) (>= i x))
            (incf (aref dp i j) (aref dp (- i x) (1- j)))))))
    
    (format t "Calculating complement configurations G(n)...~%")
    (let ((g-n 0))
      (iterate (for k from 1 to n)
        (let ((a-nk (aref dp n k))
              (a-nk-1 (aref dp n (1- k))))
          ;; Rスタート/B終わりの対称パターン (Rがk個、Bがk個) x 2
          (incf g-n (* 2 a-nk a-nk))
          ;; Rスタート/R終わり と Bスタート/B終わりのパターン (差が1個) x 2
          (incf g-n (* 2 a-nk a-nk-1))))
      
      (let* ((total-configs (choose (* 2 n) n))
             (ans (- total-configs g-n)))
        (format t "Total configurations: ~A~%" total-configs)
        (format t "Configurations without block size 2: ~A~%" g-n)
        (format t "Finished. Answer: ~A~%" ans)
        ans))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting DP for N=26 (Calculating partitions without block size 2)...
Calculating complement configurations G(n)...
Total configurations: 495918532948104
Configurations without block size 2: 349537452378
Finished. Answer: 495568995495726

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 6408 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 495568995495726
:ok