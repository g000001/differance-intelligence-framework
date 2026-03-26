;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0821 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0821)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (inline phi))
(defun phi (x)
  "Calculates the number of integers <= x that are coprime to 6 in O(1)."
  (declare (type (unsigned-byte 64) x))
  (multiple-value-bind (q r) (truncate x 6)
    (the (unsigned-byte 64)
         (+ (the (unsigned-byte 64) (* 2 q))
            (if (>= r 1) 1 0)
            (if (>= r 5) 1 0)))))

(defun generate-valid-masks (length)
  "Generates all valid masks (no adjacent 1s) of given length to compress DP space to Fibonacci bounds."
  (let ((res (make-array 0 :element-type '(unsigned-byte 64) :adjustable t :fill-pointer 0)))
    (labels ((rec (idx current)
               (if (= idx length)
                   (vector-push-extend current res)
                   (progn
                     ;; We can always skip (place 0)
                     (rec (1+ idx) current)
                     ;; We can place 1 only if the previous bit is not 1
                     (when (or (= idx 0) (not (logbitp (1- idx) current)))
                       (rec (1+ idx) (logior current (ash 1 idx))))))))
      (rec 0 0))
    res))

(defun solve-dp-for-shape (K)
  "Executes profile DP restricted to valid Fibonacci masks on the V_K triangular graph."
  (declare (type (unsigned-byte 64) K))
  (let ((b-limits (make-array 60 :element-type 'fixnum :initial-element -1))
        (max-a -1))
    ;; 1. Kにおけるグラフの形状（各列 a の高さ b）を計算
    (loop for a from 0 to 55 do
      (let ((pow2 (expt 2 a)))
        (if (> pow2 K)
            (return)
            (let ((max-b -1))
              (loop for b from 0 to 35 do
                (if (<= (expt 3 b) (truncate K pow2))
                    (setf max-b b)
                    (return)))
              (setf (aref b-limits a) max-b)
              (setf max-a a)))))
              
    ;; 2. プロファイルDP
    (let ((dp (make-hash-table :test 'eql))
          (next-dp (make-hash-table :test 'eql)))
      (setf (gethash 0 dp) 0)
      
      (loop for a from 0 to max-a do
        (clrhash next-dp)
        (let* ((b-limit (aref b-limits a))
               ;; Generate valid placement masks on the fly for the exact column height
               (valid-masks (if (>= b-limit 0) (generate-valid-masks (1+ b-limit)) nil)))
          (when (>= b-limit 0)
            (maphash
             (lambda (mask score)
               (declare (type (unsigned-byte 64) mask score))
               (loop for i from 0 below (length valid-masks) do
                 (let ((placed (aref valid-masks i)))
                   (declare (type (unsigned-byte 64) placed))
                   ;; 制約: 前列からの mask と衝突しない
                   (when (and (zerop (logand placed mask))
                              (zerop (logand (ash placed 1) mask)))
                     (let* ((cov (logior mask placed (ash placed 1)))
                            (valid-mask (1- (ash 1 (1+ b-limit))))
                            (pts (logcount (logand cov valid-mask)))
                            (new-mask placed)
                            (current-best (gethash new-mask next-dp -1)))
                       (when (> (+ score pts) current-best)
                         (setf (gethash new-mask next-dp) (+ score pts))))))))
             dp)))
        (let ((tmp dp)) (setf dp next-dp) (setf next-dp tmp)))
        
      ;; DPの最終結果の最大値を取得
      (let ((max-score 0))
        (maphash (lambda (m s) (declare (ignore m)) (setf max-score (max max-score s))) dp)
        max-score))))

(defun solve-for (n)
  (declare (type (unsigned-byte 64) n))
  (let ((v-list (make-array 0 :element-type '(unsigned-byte 64) :adjustable t :fill-pointer 0)))
    ;; 全ての v = 2^a 3^b <= n を列挙
    (loop for a from 0 to 55 do
      (let ((pow2 (expt 2 a)))
        (if (> pow2 n)
            (return)
            (loop for b from 0 to 35 do
              (let ((pow3 (expt 3 b)))
                (if (> pow3 (truncate n pow2))
                    (return)
                    (vector-push-extend (* pow2 pow3) v-list)))))))
                    
    ;; 【致命的ミスの修正】 昇順（ASCENDING）ソートによって区間の包含関係を正確にトレースする
    (setf v-list (sort v-list #'<))
    
    (let ((total-ans 0)
          (memo (make-hash-table :test 'eql)))
      ;; 各形状区間について DP を回す
      (loop for i from 0 below (length v-list) do
        (let* ((v (aref v-list i))
               ;; v_i <= n/m < v_{i+1} となる m の個数を抽出
               (count (if (= i (1- (length v-list)))
                          (phi (truncate n v))
                          (- (phi (truncate n v)) (phi (truncate n (aref v-list (1+ i))))))))
          (when (> count 0)
            (let ((fk (gethash v memo)))
              (unless fk
                (setf fk (solve-dp-for-shape v))
                (setf (gethash v memo) fk))
              (incf total-ans (* count fk))))))
      total-ans)))

(defun solve ()
  (format t "Validating with F(6)...~%")
  (let ((ans6 (solve-for 6)))
    (format t "F(6) = ~A (Expected 5)~%" ans6)
    (assert (= ans6 5)))
    
  (format t "Validating with F(20)...~%")
  (let ((ans20 (solve-for 20)))
    (format t "F(20) = ~A (Expected 19)~%" ans20)
    (assert (= ans20 19)))
    
  (format t "Computing F(10^16) with Exact Graph Profile DP...~%")
  (let ((ans (solve-for (expt 10 16))))
    (format t "Final Answer F(10^16) = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0821:solve)