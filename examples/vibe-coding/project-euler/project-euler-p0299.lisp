;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0299 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0299)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(defun gcd-fast (a b)
  (declare (type fixnum a b))
  (iterate (while (not (zerop b)))
           (let ((temp b))
             (setf b (mod a b))
             (setf a temp)))
  a)

(defun solve ()
  (let* (($limit #.(expt 10 8))
         ($ans 0)
         ;; それぞれの生成式の最大限界を見積もり
         ($m-limit-1 (isqrt (floor $limit 2)))
         ($m-limit-2 (isqrt (floor $limit 4))))
    
    (format t "Step 1: Family 1 [x=y, u!=v] の探索を開始...~%")
    ;; S1 = 2m^2 + n^2 + 4mn (n は奇数)
    ;; u と v が異なるため、(a,b,d) と (a,d,b) の2つの組が生成される (× 2)
    (iterate (for m from 1 to $m-limit-1)
             (declare (type fixnum m))
             (iterate (for n from 1 by 2)
                      (declare (type fixnum n))
                      (let ((s1 (+ (* 2 m m) (* n n) (* 4 m n))))
                        (declare (type fixnum s1))
                        ;; 限界を超えたら内側ループを抜ける (枝刈り)
                        (when (>= s1 $limit) (leave))
                        
                        (when (= 1 (gcd-fast m n))
                          (incf $ans (* 2 (floor (1- $limit) s1)))))))
                          
    (format t "Step 2: Family 2 [x!=y, u=v] の探索を開始...~%")
    ;; S3 = 4m^2 + 2n^2 + 4mn (n は奇数)
    ;; u と v が等しい (b=d) ため、対称性による増加はない (× 1)
    (iterate (for m from 1 to $m-limit-2)
             (declare (type fixnum m))
             (iterate (for n from 1 by 2)
                      (declare (type fixnum n))
                      (let ((s3 (+ (* 4 m m) (* 2 n n) (* 4 m n))))
                        (declare (type fixnum s3))
                        (when (>= s3 $limit) (leave))
                        
                        (when (= 1 (gcd-fast m n))
                          (incf $ans (floor (1- $limit) s3))))))
    
    (format t "Step 3: 独立した解族の合流と集計が完了しました。~%")
    (format t "Final Result (L=~A): ~A~%" $limit $ans)
    $ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: Family 1 [x=y, u!=v] の探索を開始...
Step 2: Family 2 [x!=y, u=v] の探索を開始...
Step 3: 独立した解族の合流と集計が完了しました。
Final Result (L=100000000): 549936643

User time    =        2.891
System time  =        0.031
Elapsed time =        2.842
Allocation   = 1067936 bytes
3853 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 549936643
:ok