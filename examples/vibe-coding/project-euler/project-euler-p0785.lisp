;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0785 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0785)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(defconstant $const-limit #.(expt 10 9))
(defconstant $const-stack-size 65536)

(defmacro push-node-to-stack (m1-array n1-array m2-array n2-array sp a b c d)
  `(progn
     (setf (aref ,m1-array ,sp) ,a
           (aref ,n1-array ,sp) ,b
           (aref ,m2-array ,sp) ,c
           (aref ,n2-array ,sp) ,d)
     (incf ,sp)))

(defun solve ()
  (format t "Starting PE 785...~%")
  (let ((val-total 0)
        ;; ヒープGCを回避し、再帰によるスタックオーバーフローを防ぐための静的配列スタック
        (stack-m1 (make-array $const-stack-size :element-type '(unsigned-byte 32)))
        (stack-n1 (make-array $const-stack-size :element-type '(unsigned-byte 32)))
        (stack-m2 (make-array $const-stack-size :element-type '(unsigned-byte 32)))
        (stack-n2 (make-array $const-stack-size :element-type '(unsigned-byte 32)))
        (stack-pointer 0))
    
    ;; 探索木の初期ノード (1, 1) を手動で評価 (端点の0/1と3/2はx,z=0を含むため除外)
    (let ((init-m 1)
          (init-n 1))
      (when (<= (* (+ init-m init-n) (+ (* 3 init-m) (* 5 init-n))) $const-limit)
        (when (/= (mod init-m 19) (mod (* 11 init-n) 19))
          (incf val-total (* 8 (+ (* init-m init-m) (* init-m init-n) (* init-n init-n)))))))
    
    ;; Stern-Brocot木の初期区間 (0/1 〜 1/1) と (1/1 〜 3/2) をスタックに積む
    (push-node-to-stack stack-m1 stack-n1 stack-m2 stack-n2 stack-pointer 0 1 1 1)
    (push-node-to-stack stack-m1 stack-n1 stack-m2 stack-n2 stack-pointer 1 1 3 2)
    
    (iterate
      (while (> stack-pointer 0))
      (decf stack-pointer)
      (for val-m1 = (aref stack-m1 stack-pointer))
      (for val-n1 = (aref stack-n1 stack-pointer))
      (for val-m2 = (aref stack-m2 stack-pointer))
      (for val-n2 = (aref stack-n2 stack-pointer))
      
      ;; Farey加算による中点 (m, n) の生成 (これによりmとnは必ず互いに素になる)
      (for iter-m = (+ val-m1 val-m2))
      (for iter-n = (+ val-n1 val-n2))
      
      ;; 最大要素 Z <= Limit の枝刈り判定
      (when (<= (* (+ iter-m iter-n) (+ (* 3 iter-m) (* 5 iter-n))) $const-limit)
        ;; 重複解 (d=19) を生成するパラメータの除外
        (when (/= (mod iter-m 19) (mod (* 11 iter-n) 19))
          (incf val-total (* 8 (+ (* iter-m iter-m) (* iter-m iter-n) (* iter-n iter-n)))))
        
        ;; 深さ優先で子区間をスタックに積む
        (push-node-to-stack stack-m1 stack-n1 stack-m2 stack-n2 stack-pointer val-m1 val-n1 iter-m iter-n)
        (push-node-to-stack stack-m1 stack-n1 stack-m2 stack-n2 stack-pointer iter-m iter-n val-m2 val-n2)))
    
    (format t "Done. The answer is: ~A~%" val-total)
    val-total))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting PE 785...
Done. The answer is: 29526986315080920

User time    =        1.478
System time  =        0.010
Elapsed time =        1.460
Allocation   = 1095368 bytes
546 Page faults
GC time      =        0.001
 |------------------------------------------------------------|#
;;→ 29526986315080920
:ok