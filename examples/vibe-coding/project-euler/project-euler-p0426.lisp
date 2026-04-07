;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0426 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0426)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)

(defun solve (&optional (n 10000000))
  (declare (type fixnum n))
  (format t "Phase 1: Generating Box-Ball Sequence...~%")
  
  (let ((t-array (make-array (1+ n) :element-type 'fixnum))
        (s 290797))
    (declare (type (unsigned-byte 62) s)
             (type (simple-array fixnum (*)) t-array))
    
    (iterate (for i from 0 to n)
      (setf (aref t-array i) (+ (logand s 63) 1))
      (setf s (mod (* s s) 50515093)))
      
    (format t "Phase 2: Extracting Solitons via KKR Crystal Basis Stack...~%")
    ;; スタックは (ボールの残り数, ベースとなるネストの深さ) を記録する
    (let ((stack-count (make-array (+ (truncate n 2) 2) :element-type 'fixnum))
          (stack-depth (make-array (+ (truncate n 2) 2) :element-type 'fixnum))
          (top -1)
          (sum-sq 0))
      (declare (type fixnum top)
               (type integer sum-sq) ; 最終和は非常に大きくなるため bignum 安全を確保
               (type (simple-array fixnum (*)) stack-count stack-depth))
               
      (iterate (for i from 0 to (truncate n 2))
        (let ((b (aref t-array (* 2 i)))
              ;; 最後のブロックの後には無限の空き箱があるとみなす
              (e (if (< (* 2 i) n)
                     (aref t-array (1+ (* 2 i)))
                     MOST-POSITIVE-FIXNUM)))
          (declare (type fixnum b e))
          
          ;; ボールのブロック(開き括弧)をスタックに積む
          (when (> b 0)
            (incf top)
            (setf (aref stack-count top) b)
            (setf (aref stack-depth top) 0))
            
          ;; 空き箱のブロック(閉じ括弧)で、スタック上のボールとマッチングさせる
          (iterate (while (and (> e 0) (>= top 0)))
            (let ((k (aref stack-count top))
                  (d (aref stack-depth top)))
              (declare (type fixnum k d))
              
              (if (<= k e)
                  ;; トップのボールブロックが全てマッチングされる場合
                  (progn
                    (decf e k)
                    (decf top)
                    (let ((passed-depth (+ d k)))
                      (declare (type fixnum passed-depth))
                      ;; 2dk + k^2 の公式による O(1) 更新
                      (incf sum-sq (+ (* 2 (the integer d) (the integer k))
                                      (* (the integer k) (the integer k))))
                      ;; マッチした括弧群は、親(スタックの次の要素)の深さを押し上げる
                      (when (>= top 0)
                        (setf (aref stack-depth top)
                              (max (aref stack-depth top) passed-depth)))))
                  
                  ;; トップのボールブロックが部分的にマッチングされ残る場合
                  (progn
                    (setf (aref stack-count top) (- k e))
                    (let ((passed-depth (+ d e)))
                      (declare (type fixnum passed-depth))
                      (incf sum-sq (+ (* 2 (the integer d) (the integer e))
                                      (* (the integer e) (the integer e))))
                      ;; 残ったボールは親となるため、深さを引き継ぐ
                      (setf (aref stack-depth top) passed-depth)
                      (setf e 0))))))))
                      
      (format t "Target Initial Re-eval (N=10): F(10) = 8912 ? => ~A~%" 
              (if (= n 10) sum-sq "Skipped. (Try (solve 10) to verify)"))
      (format t "Result: F(~A) = ~A~%" n sum-sq)
      sum-sq)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Phase 1: Generating Box-Ball Sequence...
Phase 2: Extracting Solitons via KKR Crystal Basis Stack...
Target Initial Re-eval (N=10): F(10) = 8912 ? => Skipped. (Try (solve 10) to verify)
Result: F(10000000) = 31591886008

User time    =        0.292
System time  =        0.041
Elapsed time =        0.275
Allocation   = 160363736 bytes
23238 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 31591886008
:ok