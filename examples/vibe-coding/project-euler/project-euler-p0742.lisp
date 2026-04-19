;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0742 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0742)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defun get-pool (limit)
  "指定した和の範囲内で既約ベクトル (x, y) のプールを生成"
  (let ((pool nil))
    (iterate (for s from 2 to limit)
             (iterate (for x from 1 below s)
                      (let ((y (- s x)))
                        (when (= 1 (gcd x y))
                          (push (cons x y) pool)))))
    (nreverse pool)))

(defun fast-area (vectors)
  "対称凸格子多角形の面積を、m個のベクトルから整数演算のみで計算する (A(4)=1, A(8)=7 を満たす公式)"
  (declare (optimize (speed 3) (safety 0)))
  ;; 面積を最小化するためには、ベクトルを傾き y/x の昇順にソートする必要がある
  (let* ((sorted (sort (copy-list vectors) 
                       (lambda (v1 v2) 
                         (< (* (the fixnum (cdr v1)) (the fixnum (car v2))) 
                            (* (the fixnum (cdr v2)) (the fixnum (car v1)))))))
         (x-sum 0) (y-sum 0) (xy-sum 0) (cross-sum 0) (y-acc 0))
    (declare (type integer x-sum y-sum xy-sum cross-sum y-acc))
    
    (iterate (for v in sorted)
             (let ((x (car v)) (y (cdr v)))
               (declare (type fixnum x y))
               (incf x-sum x)
               (incf y-sum y)
               (incf xy-sum (* x y))))
    
    (iterate (for v in sorted)
             (let ((x (car v)) (y (cdr v)))
               (declare (type fixnum x y))
               ;; x_i * (自分より後の y_j の合計)
               (incf cross-sum (* x (- y-sum y-acc y)))
               (incf y-acc y)))
    
    ;; A = 1 + 2X + 2Y + 2Σxy + 4Σ(x_i * Y_suffix)
    (+ 1 (* 2 x-sum) (* 2 y-sum) (* 2 xy-sum) (* 4 cross-sum))))

(defun solve ()
  (let* ((m 249) ;; N=1000 の時 m=(1000-4)/4 = 249
         (pool (get-pool 120)) ;; 探索空間を十分に広く取る
         ;; 初期解：L1ノルム (x+y) が小さい順に 249 個選ぶ
         (current (subseq (sort (copy-list pool) 
                                (lambda (v1 v2) (< (+ (car v1) (cdr v1)) (+ (car v2) (cdr v2))))) 
                          0 m))
         (in-set (make-hash-table :test 'equal))
         (current-area 0))
    
    (dolist (v current) (setf (gethash v in-set) t))
    (setf current-area (fast-area current))
    
    (format t "Step 1: 初期解 (Greedy L1) の面積を算出: ~A~%" current-area)
    (format t "Step 2: 厳密な 1-swap 局所探索による最適化を開始...~%")
    
    (let ((improved t))
      (iterate (while improved)
               (setf improved nil)
               ;; 現在の集合に含まれるベクトル v-out を、プール内の v-in と入れ替えて面積が減るか試行
               (iterate (for v-out in current)
                        (let ((base-list (remove v-out current)))
                          (iterate (for v-in in pool)
                                   (unless (gethash v-in in-set)
                                     (let* ((next-list (cons v-in base-list))
                                            (next-area (fast-area next-list)))
                                       (when (< next-area current-area)
                                         (setf current-area next-area)
                                         (setf current next-list)
                                         (remhash v-out in-set)
                                         (setf (gethash v-in in-set) t)
                                         (setf improved t)
                                         (format t "  [Update] Area reduced to ~A~%" current-area)
                                         (leave))))))
                        (when improved (leave)))))
    
    (format t "~%Final Result A(1000): ~A~%" current-area)
    current-area))