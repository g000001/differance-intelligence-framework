;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0961 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0961)

(defun solve (&optional (n-limit 18))
  (format t "Starting Dimensional Collapse (Bitmask DP) for 10^~A...~%" n-limit)
  (let* ((max-x (ash 1 n-limit))
         ;; 状態空間を完全にカバーする極小のビット配列 (わずか32KB)
         (dp (make-array max-x :element-type 'bit :initial-element 0))
         (ans 0)
         ;; 9の冪乗を事前計算し、ループ内のBignum演算を回避
         (pow9 (make-array (1+ n-limit) :element-type 'integer)))
    
    (iterate (for i from 0 to n-limit)
      (setf (aref pow9 i) (expt 9 i)))
    
    ;; 状態空間 x=1 から 2^18-1 までの全探索
    (iterate (for x from 1 below max-x)
      (when (= (mod x 50000) 0)
        (format t "Progress: ~A / ~A~%" x max-x))
      
      (let ((win 0)
            (len (integer-length x)))
        
        ;; i番目のビット（桁）を取り除くすべての遷移を評価
        (iterate (for i from 0 below len)
          (let* ((higher (ash x (- (1+ i))))
                 (lower (logand x (1- (ash 1 i))))
                 ;; i番目のビットを抜き、上位ビットと下位ビットを結合する
                 ;; ※ Lispの整数として扱うことで leading zeros は自動的に消滅・正規化される
                 (next-x (logior (ash higher i) lower)))
            
            ;; 遷移先が一つでも L (0) なら、現在の状態は W (1) になる
            (when (= (aref dp next-x) 0)
              (setf win 1)
              (finish))))
        
        (setf (aref dp x) win)
        
        ;; W (1) である場合、このビット列に合致する「実際の10進数の個数」を解に加算
        (when (= win 1)
          (incf ans (aref pow9 (logcount x))))))
      
    (format t "Finished. Answer: ~A~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Dimensional Collapse (Bitmask DP) for 10^18...
Progress: 50000 / 262144
Progress: 100000 / 262144
Progress: 150000 / 262144
Progress: 200000 / 262144
Progress: 250000 / 262144
Finished. Answer: 166666666689036288

User time    =        0.154
System time  =        0.010
Elapsed time =        0.111
Allocation   = 129744 bytes
287 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 166666666689036288
:ok