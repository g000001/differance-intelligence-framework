;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0186 (:use cl #:iterate))
(in-package #:project-euler-0186)

#||
(cl-text 
 (cl-comment "Project Euler P186: Connectedness in a Lagged Fibonacci Network")

 ;; 1. 領域の定義 (Domain Definitions)
 (domain User (integer-range 0 999999))
 (object PM (User 524287))
 (object TotalUsers (integer 1000000))
 (object TargetThreshold (decimal 0.99))

 ;; 2. 状態空間 (State Space: Conventional Truth)
 (cl-comment "電話網の状態は、連結成分の集合として表現される（世俗諦）")
 (structure NetworkState
    (components (Partition User))
    (successful-calls integer)
    (pm-component-size (lambda (state) (size (find-component PM (components state))))))

 ;; 3. 遷移規則 (Transition Rules: Dependent Origination)
 (cl-comment "LFGによるコールの発生と、それによるネットワークの変容（縁起）")
 (function LFG (k) 
    (if (<= k 55)
        (mod (+ 100003 (* -200003 k) (* 300007 (expt k 3))) 1000000)
        (mod (+ (LFG (- k 24)) (LFG (- k 55))) 1000000)))

 (rule CallTransition (n state)
    (let ((caller (LFG (- (* 2 n) 1)))
          (called (LFG (* 2 n))))
      (if (= caller called)
          state ;; Misdial: No change
          (update-state state 
            (components (union (components state) caller called))
            (successful-calls (+ (successful-calls state) 1))))))

 ;; 4. 不動点への収束 (Convergence to Fixed Point: Ultimate Truth)
 (cl-comment "PMの連結成分が全ユーザーの99%に達した瞬間を勝義諦とする")
 (goal IsConverged (state)
    (>= (/ (pm-component-size state) TotalUsers) TargetThreshold))

 ;; 5. 最適化戦略 (Optimization: ACX Jump)
 (cl-comment "全探索を避け、Disjoint Set Union (DSU) による効率的な計算への跳躍")
 (strategy DSU-Implementation
    (path-compression true)
    (union-by-size true)
    (complexity (O (* N (inverse-ackermann N)))))
)
||#

(defun solve-p186 ()
  (let* ((num-users 1000000)
         (pm 524287)
         (target-size 990000)
         (parent (make-array num-users :element-type '(unsigned-byte 32)))
         (sz (make-array num-users :element-type '(unsigned-byte 32) :initial-element 1))
         (lfg-buf (make-array 55 :element-type '(unsigned-byte 32)))
         (lfg-ptr 0)
         (success-calls 0))

    (iter (for i from 0 below num-users) (setf (aref parent i) i))

    (labels ((find-root (i)
               (let ((p (aref parent i)))
                 (if (= p i)
                     i
                     (setf (aref parent i) (find-root p)))))
             (unite (i j)
               (let ((r1 (find-root i))
                     (r2 (find-root j)))
                 (if (/= r1 r2)
                     (progn
                       (if (< (aref sz r1) (aref sz r2)) (rotatef r1 r2))
                       (setf (aref parent r2) r1)
                       (incf (aref sz r1) (aref sz r2))
                       t)
                     nil))))

      ;; Initial 55
      (iter (for k from 1 to 55)
        (setf (aref lfg-buf (1- k))
              (mod (+ 100003 (* -200003 k) (* 300007 k k k)) 1000000)))

      (iter (for n from 1)
        (flet ((gen-s ()
                 (let* ((k (+ (* 2 n) (if (evenp lfg-ptr) -1 0))) ;; Conceptual, handled by ptr
                        (idx lfg-ptr)
                        (val (if (< n 28) ;; First 55 S values are generated in n=1..27.5
                                 (aref lfg-buf idx) ;; This logic is messy, let's use a flat counter
                                 nil)))
                   ;; Simplified: Use a global S counter
                   nil)))
          ;; Resetting generator logic for clarity
          (let* ((s-counter 1)
                 (get-s (lambda ()
                          (let ((res (if (<= s-counter 55)
                                         (aref lfg-buf (1- s-counter))
                                         (let ((v (mod (+ (aref lfg-buf (mod (- s-counter 24 1) 55))
                                                          (aref lfg-buf (mod (- s-counter 55 1) 55)))
                                                       1000000)))
                                           (setf (aref lfg-buf (mod (1- s-counter) 55)) v)
                                           v))))
                            (incf s-counter)
                            res))))
                
            (iter (for call-nr from 1)
              (let ((u1 (funcall get-s))
                    (u2 (funcall get-s)))
                (unless (= u1 u2)
                  (incf success-calls)
                  (unite u1 u2)
                  (when (>= (aref sz (find-root pm)) target-size)
                    (return-from solve-p186 success-calls)))))))))))

#+| Do it | (solve-p186 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-p186)

User time    =        0.939
System time  =        0.013
Elapsed time =        0.899
Allocation   = 9575616 bytes
1955 Page faults
GC time      =        0.001
 |------------------------------------------------------------|#
;;→ 2325629


;; 感想：
;; CLIFによる分析を事前に行うことで、この問題の本質が「単なるシミュレーション」ではなく、
;; 「巨大なグラフにおける連結成分の動的収束」であることを再認識できました。
;; 特に、DSU（Union-Find）のパス圧縮とサイズによる統合（ACX Jump）の必要性が、
;; CLIFの「Ultimate Truth」の定義から自然に導かれ、実装の迷いがなくなりました。
;; また、LFGのバッファ管理についても、CLIFで「Dependent Origination（縁起）」として
;; 状態遷移を記述したことで、リングバッファ的な更新ロジックを正確に捉えることができました。
;; iterateの使用は、CLIFでの「不動点への収束」を表現するのに、loopよりも構造的で適していると感じました。

:ok