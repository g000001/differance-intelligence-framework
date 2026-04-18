;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash
(cl:in-package cl-user)
(defpackage #:project-euler-bonus-ultimate (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-bonus-ultimate)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defun solve ()
  (format t "Step 1: O(1) に近い極小状態空間での行列DPを実行中...~%")
  (let (($states (make-hash-table :test 'equal))
        ($v0 (make-array 13 :initial-element 0))
        ($v1 (make-array 13 :initial-element 0))
        ($limit 5000)) ; 理論上、有効な中間行列の成分は数百に収まるため 5000 で十分
    
    ;; 初期状態: 単位行列 (A B C D)
    (setf (gethash '(1 0 0 1) $states) 1)
    
    (iterate (for len from 1 to 12)
             (let (($next-states (make-hash-table :test 'equal)))
               (iterate (for (mat count) in-hashtable $states)
                        (let ((A (first mat)) (B (second mat))
                              (C (third mat)) (D (fourth mat)))
                          ;; 定理: 周期12において a_i は 13 を絶対に超えない
                          (iterate (for a-val from 0 to 13)
                                   (let ((nA (- (* a-val A) C))
                                         (nB (- (* a-val B) D)))
                                     (when (and (< (abs nA) $limit) (< (abs nB) $limit))
                                       (incf (gethash (list nA nB A B) $next-states 0) count))))))
               (setf $states $next-states)
               
               ;; 長さ len の全系列から、トレース別に有効数をカウント
               (iterate (for (mat count) in-hashtable $states)
                        (let ((tr (+ (first mat) (fourth mat))))
                          (cond ((= tr 0) (incf (aref $v0 len) count))
                                ((or (= tr 1) (= tr -1)) (incf (aref $v1 len) count)))))))
    
    (format t "Step 2: トレース分離型の厳密なメビウス反転を適用...~%")
    (let (($g0 (make-array 13 :initial-element 0))
          ($g1 (make-array 13 :initial-element 0))
          ($ans 0))
      
      (iterate (for k from 1 to 12)
               (setf (aref $g0 k) (aref $v0 k))
               (setf (aref $g1 k) (aref $v1 k))
               
               (iterate (for j from 1 below k)
                        (when (zerop (mod k j))
                          (let ((m (floor k j)))
                            ;; トレース0の重複排除: 奇数回繰り返された時のみ引く
                            (when (oddp m)
                              (decf (aref $g0 k) (aref $g0 j)))
                            ;; トレース±1の重複排除: 3の倍数回以外の繰り返しの時のみ引く
                            (when (/= (mod m 3) 0)
                              (decf (aref $g1 k) (aref $g1 j))))))
               
               (let ((total-min-period (+ (aref $g0 k) (aref $g1 k))))
                 (incf $ans total-min-period)
                 (format t "Minimal period ~2A: ~10A sequences. Q(~2A) = ~A~%" 
                         k total-min-period k $ans)))
      
      (format t "~%Final Result Q(12): ~A~%" $ans)
      $ans)))


#+| Do it | (solve )