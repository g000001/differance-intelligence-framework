;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0078 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0078)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)

(defconstant $limit-mod #.(expt 10 6))
;; N = 100,000 をカバーするには、k = 260 付近まで必要なので 1000 個あれば十分
(defconstant $limit-pentagonals 1000)

(defun precompute-pentagonals (limit-count)
  "一般五角数を事前計算し、固定長ベクタとして返す"
  (let ((pentagonals (make-array limit-count :element-type 'integer :initial-element 0)))
    (iterate
      (for index from 0 below limit-count)
      ;; index = 0, 1, 2, 3... に対して k-base = 1, 1, 2, 2...
      (for k-base = (1+ (truncate index 2)))
      ;; 偶数インデックスなら正、奇数インデックスなら負
      (for k-val = (if (evenp index) k-base (- k-base)))
      ;; g_k = k(3k - 1) / 2
      (for g-offset = (truncate (* k-val (1- (* 3 k-val))) 2))
      (setf (aref pentagonals index) g-offset))
    pentagonals))

(defun solve ()
  "Find the least value of n for which p(n) is divisible by 1,000,000."
  (let* ((initial-capacity #.(expt 10 5))
         (partition-storage (make-array initial-capacity :element-type 'integer :initial-element 0))
         (pentagonals (precompute-pentagonals $limit-pentagonals)))
    
    (setf (aref partition-storage 0) 1)

    (format t "Starting optimized search for p(n) ≡ 0 (mod ~D)...~%" $limit-mod)

    (iterate
      (for current-n from 1)
      
      ;; 探索空間の動的拡張
      (when (>= current-n (length partition-storage))
        (let ((new-storage (make-array (* 2 (length partition-storage)) :element-type 'integer :initial-element 0)))
          (replace new-storage partition-storage)
          (setf partition-storage new-storage)
          (format t "Expanded partition-storage to ~D~%" (length partition-storage))))

      (let ((current-p 0))
        ;; 最深部のループ: modを排除し、事前計算された配列へのアクセスのみに還元
        (iterate
          (for index from 0 below $limit-pentagonals)
          (for g-offset = (aref pentagonals index))
          
          (when (> g-offset current-n)
            (terminate))
          
          (let ((term (aref partition-storage (- current-n g-offset))))
            ;; index の 2ビット目 (値2) が 0 なら加算、1 なら減算
            ;; これにより +, +, -, -, +, + ... の周期を O(1) かつ除算なしで判定
            (if (zerop (logand index 2))
                (incf current-p term)
                (decf current-p term))))
        
        ;; ループ終了後に一度だけ剰余を適用し、負数の場合は正の剰余に補正
        (let ((final-p (mod current-p $limit-mod)))
          (setf (aref partition-storage current-n) final-p)
          
          (when (zerop final-p)
            (format t "Found: p(~D) is divisible by ~D~%" current-n $limit-mod)
            (return current-n))
          
          ;; 1万件ごとのトレース出力
          (when (zerop (mod current-n #.(expt 10 4)))
            (format t "Tracing: n = ~D, p(n) mod 10^6 = ~D~%" 
                    current-n final-p)))))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting optimized search for p(n) ≡ 0 (mod 1000000)...
Tracing: n = 10000, p(n) mod 10^6 = 435144
Tracing: n = 20000, p(n) mod 10^6 = 113097
Tracing: n = 30000, p(n) mod 10^6 = 192786
Tracing: n = 40000, p(n) mod 10^6 = 469797
Tracing: n = 50000, p(n) mod 10^6 = 548263
Found: p(55374) is divisible by 1000000

User time    =        0.130
System time  =        0.009
Elapsed time =        0.086
Allocation   = 912896 bytes
313 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 55374
:ok