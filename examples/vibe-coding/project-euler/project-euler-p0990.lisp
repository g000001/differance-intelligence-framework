;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0990 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0990)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant $modulus 1000000007)

(declaim (inline make-fixnum-array))
(defun make-fixnum-array (size &key (initial-element 0))
  (make-array size :element-type 'fixnum :initial-element initial-element))

(defun precompute-ways ()
  "桁の生成パターンと配置（組み合わせ）を事前計算して ways-table を返す"
  (let ((ways-end (make-fixnum-array '(26 250)))
        (ways-cont (make-fixnum-array '(26 250)))
        (combinations (make-fixnum-array '(26 26)))
        (ways-table (make-fixnum-array '(26 26 250))))
    
    ;; 終了する項の生成パターン (1〜9を割り当て)
    (setf (aref ways-end 0 0) 1)
    (iterate ((idx (scan-range :from 1 :upto 25)))
      (iterate ((sum-val (scan-range :from 1 :upto 225)))
        (iterate ((digit (scan-range :from 1 :upto 9)))
          (when (>= sum-val digit)
            (setf (aref ways-end idx sum-val)
                  (mod (+ (aref ways-end idx sum-val) 
                          (aref ways-end (1- idx) (- sum-val digit))) 
                       $modulus))))))
                  
    ;; 継続する項の生成パターン (0〜9を割り当て)
    (setf (aref ways-cont 0 0) 1)
    (iterate ((idx (scan-range :from 1 :upto 25)))
      (iterate ((sum-val (scan-range :from 0 :upto 225)))
        (iterate ((digit (scan-range :from 0 :upto 9)))
          (when (>= sum-val digit)
            (setf (aref ways-cont idx sum-val)
                  (mod (+ (aref ways-cont idx sum-val) 
                          (aref ways-cont (1- idx) (- sum-val digit))) 
                       $modulus))))))
                  
    ;; 二項係数 C(n, k)
    (iterate ((n-val (scan-range :from 0 :upto 25)))
      (setf (aref combinations n-val 0) 1)
      (iterate ((k-val (scan-range :from 1 :upto n-val)))
        (setf (aref combinations n-val k-val)
              (mod (+ (aref combinations (1- n-val) (1- k-val)) 
                      (aref combinations (1- n-val) k-val)) 
                   $modulus))))
              
    ;; 組み合わせと和の畳み込み
    (iterate ((total-count (scan-range :from 0 :upto 25)))
      (iterate ((end-count (scan-range :from 0 :upto total-count)))
        (let ((cont-count (- total-count end-count)))
          (iterate ((sum-end (scan-range :from 0 :upto (* 9 end-count))))
            (let ((ways-e (aref ways-end end-count sum-end)))
              (when (> ways-e 0)
                (iterate ((sum-cont (scan-range :from 0 :upto (* 9 cont-count))))
                  (let ((ways-c (aref ways-cont cont-count sum-cont)))
                    (when (> ways-c 0)
                      (let ((total-sum (+ sum-end sum-cont)))
                        (setf (aref ways-table total-count end-count total-sum)
                              (mod (+ (aref ways-table total-count end-count total-sum)
                                      (mod (* (aref combinations total-count end-count)
                                              (mod (* ways-e ways-c) $modulus))
                                           $modulus))
                                   $modulus))))))))))))
    ways-table))

(defun solve-dp (max-n ways-table)
  "事前計算された ways-table を用いて Digit DP を実行する"
  (let ((dp-table (make-fixnum-array '(26 26 51 51)))
        (dp-half (make-fixnum-array '(26 276))))
    
    ;; 状態の初期化 (+ と = のコストを事前加算)
    (iterate ((count-l (scan-range :from 1 :upto 25)))
      (iterate ((count-r (scan-range :from 1 :upto 25)))
        (let ((init-cost (- (+ count-l count-r) 1)))
          (when (<= init-cost max-n)
            (incf (aref dp-table count-l count-r 25 init-cost) 1)))))
            
    ;; コスト（文字列長）に基づくトポロジカルソート順でのDP遷移
    (iterate ((current-cost (scan-range :from 0 :upto max-n)))
      (iterate ((count-l (scan-range :from 0 :upto 25)))
        (iterate ((count-r (scan-range :from 0 :upto 25)))
          (unless (and (= count-l 0) (= count-r 0))
            (let ((next-cost (+ current-cost count-l count-r)))
              (when (<= next-cost max-n)
                
                ;; 修正点: dp-halfのクリア範囲を正確な最大オフセット (+ 50) に拡張
                (let ((max-carry-mid (+ 50 (* 9 count-l))))
                  (iterate ((nxt-l (scan-range :from 0 :upto count-l)))
                    (iterate ((cm (scan-range :from 0 :upto max-carry-mid)))
                      (setf (aref dp-half nxt-l cm) 0))))
                      
                (let ((has-valid-state nil))
                  ;; ステップ1: 左辺の項の更新 (Meet-in-the-Middle)
                  (iterate ((carry (scan-range :from -25 :upto 25)))
                    (let ((dp-val (aref dp-table count-l count-r (+ carry 25) current-cost)))
                      (when (> dp-val 0)
                        (setf has-valid-state t)
                        (iterate ((next-l (scan-range :from 0 :upto count-l)))
                          (iterate ((sum-l (scan-range :from 0 :upto (* 9 count-l))))
                            (let ((ways-l (aref ways-table count-l (- count-l next-l) sum-l)))
                              (when (> ways-l 0)
                                (let ((carry-mid (+ carry sum-l)))
                                  (setf (aref dp-half next-l (+ carry-mid 25))
                                        (mod (+ (aref dp-half next-l (+ carry-mid 25))
                                                (* dp-val ways-l))
                                             $modulus))))))))))
                                             
                  ;; ステップ2: 右辺の項の更新と繰り上がりの検証
                  (when has-valid-state
                    (iterate ((next-l (scan-range :from 0 :upto count-l)))
                      (iterate ((carry-mid (scan-range :from -25 :upto (+ 25 (* 9 count-l)))))
                        (let ((half-val (aref dp-half next-l (+ carry-mid 25))))
                          (when (> half-val 0)
                            (iterate ((next-r (scan-range :from 0 :upto count-r)))
                              (let ((min-c-next (ceiling (- carry-mid (* 9 count-r)) 10))
                                    (max-c-next (floor carry-mid 10)))
                                (iterate ((carry-next (scan-range :from min-c-next :upto max-c-next)))
                                  (when (and (>= carry-next -25) (<= carry-next 25))
                                    (let* ((sum-r (- carry-mid (* 10 carry-next)))
                                           (ways-r (aref ways-table count-r (- count-r next-r) sum-r)))
                                      (when (> ways-r 0)
                                        (setf (aref dp-table next-l next-r (+ carry-next 25) next-cost)
                                              (mod (+ (aref dp-table next-l next-r (+ carry-next 25) next-cost)
                                                      (* half-val ways-r))
                                                   $modulus))))))))))))))))))))
                                                   
    ;; 完成した等式（アクティブ項0、繰り上がり0）の総計
    (let ((total-answers 0))
      (iterate ((cost (scan-range :from 0 :upto max-n)))
        (setf total-answers (mod (+ total-answers (aref dp-table 0 0 25 cost)) $modulus)))
      total-answers)))

(defun solve-for (max-n)
  (let ((ways-table (precompute-ways)))
    (solve-dp max-n ways-table)))

(defun solve ()
  (format t "Verifying example A(3) = ~A (Expected: 9)~%" (solve-for 3))
  (format t "Verifying example A(5) = ~A (Expected: 171)~%" (solve-for 5))
  (format t "Verifying example A(7) = ~A (Expected: 4878)~%" (solve-for 7))
  (let ((ans (solve-for 50)))
    (format t "Final Answer for A(50) = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0990:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Verifying example A(3) = 9 (Expected: 9)
Verifying example A(5) = 171 (Expected: 171)
Verifying example A(7) = 4878 (Expected: 4878)
Final Answer for A(50) = 50322750

User time    =        2.760
System time  =        0.039
Elapsed time =        2.735
Allocation   = 62456088 bytes
13668 Page faults
GC time      =        0.003
 |------------------------------------------------------------|#
;;→ 50322750
:ok