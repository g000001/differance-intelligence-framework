;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0434 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0434)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defconstant +mod+ 1000000033)

(declaim (inline mod-add mod-sub mod-mul))
(defun mod-add (num-a num-b)
  (declare (type fixnum num-a num-b))
  (let ((sum (+ num-a num-b)))
    (if (>= sum +mod+) (- sum +mod+) sum)))

(defun mod-sub (num-a num-b)
  (declare (type fixnum num-a num-b))
  (let ((diff (- num-a num-b)))
    (if (< diff 0) (+ diff +mod+) diff)))

(defun mod-mul (num-a num-b)
  (declare (type fixnum num-a num-b))
  (mod (* num-a num-b) +mod+))

(defun solve ()
  (let* (($n 100)
         ($c-arr (make-array (* 101 101) :element-type 'fixnum :initial-element 0))
         ($p2-arr (make-array 10001 :element-type 'fixnum :initial-element 0))
         ($r-arr (make-array (* 101 101) :element-type 'fixnum :initial-element 0))
         ($ans 0))
    (declare (type fixnum $n $ans))
    
    (macrolet ((aref2 (arr r c)
                 `(aref ,arr (+ (* ,r 101) ,c))))
      
      (format t "Step 1: O(N^2) の前計算 (二項係数と2の冪乗) を実行中...~%")
      ;; 二項係数 C(n, k) の初期化
      (setf (aref2 $c-arr 0 0) 1)
      (iterate (for i from 1 to 100)
               (setf (aref2 $c-arr i 0) 1)
               (iterate (for j from 1 to i)
                        (setf (aref2 $c-arr i j) 
                              (mod-add (aref2 $c-arr (1- i) (1- j))
                                       (aref2 $c-arr (1- i) j)))))
                         
      ;; 2の冪乗の初期化
      (setf (aref $p2-arr 0) 1)
      (iterate (for i from 1 to 10000)
               (setf (aref $p2-arr i) (mod-mul (aref $p2-arr (1- i)) 2)))
        
      (format t "Step 2: O(N^4) の次元崩壊 DP を実行中...~%")
      (iterate (for m from 1 to $n)
               (iterate (for n from m to $n)
                        (let ((sum 0))
                          (declare (type fixnum sum))
                          (iterate (for i from 1 to m)
                                   (iterate (for j from 1 to n)
                                            (unless (and (= i m) (= j n))
                                              (let* ((term1 (mod-mul (aref2 $c-arr (1- m) (1- i))
                                                                     (aref2 $c-arr n j)))
                                                     (term2 (mod-mul term1 (aref2 $r-arr i j)))
                                                     (p-idx (* (- m i) (- n j)))
                                                     (term3 (mod-mul term2 (aref $p2-arr p-idx))))
                                                (setf sum (mod-add sum term3))))))
                          (let* ((tot (aref $p2-arr (* m n)))
                                 (sub (aref $p2-arr (* (1- m) n)))
                                 (val (mod-sub (mod-sub tot sub) sum)))
                            ;; 対称性を利用して R(m,n) と R(n,m) を同時更新
                            (setf (aref2 $r-arr m n) val)
                            (setf (aref2 $r-arr n m) val)
                            
                            ;; 観測点での中間ログ
                            (when (and (= m 2) (= n 3))
                              (format t "  [Debug] R(2,3) = ~A (Expected: 19)~%" val))
                            (when (and (= m 5) (= n 5))
                              (format t "  [Debug] R(5,5) = ~A (Expected: 23679901)~%" val))))))
                
      (format t "Step 3: S(N) の集計~%")
      ;; 検証用の S(5) の算出
      (let ((s5 0))
        (iterate (for i from 1 to 5)
                 (iterate (for j from 1 to 5)
                          (setf s5 (mod-add s5 (aref2 $r-arr i j)))))
        (format t "  [Debug] S(5) = ~A (Expected: 25021721)~%" s5))
        
      ;; S(100) の集計
      (iterate (for i from 1 to $n)
               (iterate (for j from 1 to $n)
                        (setf $ans (mod-add $ans (aref2 $r-arr i j)))))
          
      (format t "Final Result S(~A): ~A~%" $n $ans)
      $ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: O(N^2) の前計算 (二項係数と2の冪乗) を実行中...
Step 2: O(N^4) の次元崩壊 DP を実行中...
  [Debug] R(2,3) = 19 (Expected: 19)
  [Debug] R(5,5) = 23679901 (Expected: 23679901)
Step 3: S(N) の集計
  [Debug] S(5) = 25021721 (Expected: 25021721)
Final Result S(100): 863253606

User time    =        0.623
System time  =        0.017
Elapsed time =        0.558
Allocation   = 1311560 bytes
3817 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 863253606
:ok