;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0902 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0902)
(declaim (optimize (speed 3) (safety 0) (debug 0)))


(defconstant $m 100)
(defconstant $n 5050)
(defconstant $mod 1000000007)

(defun make-fixnum-array (size)
  (make-array size :element-type 'fixnum :initial-element 0))

(defmacro idx-w (c-x c-y d)
  `(+ (* ,c-x 10201) (* ,c-y 101) ,d))

(defmacro idx-m (a b)
  `(+ (* ,a 101) ,b))

(defun compute-gcd (a b)
  (if (= b 0)
      a
      (compute-gcd b (mod a b))))

(defun power (base-val exp-val)
  (prog ((res 1) (b (mod base-val $mod)) (e exp-val))
   loop
    (if (= e 0) (return res))
    (if (oddp e) (setf res (mod (* res b) $mod)))
    (setf b (mod (* b b) $mod))
    (setf e (ash e -1))
    (go loop)))

(defun mod-inverse (val-n)
  (power val-n (- $mod 2)))

(defun solve ()
  (let ((tau-table (make-fixnum-array (1+ $n)))
        (inv-tau-table (make-fixnum-array (1+ $n)))
        (cycle-id-table (make-fixnum-array (1+ $n)))
        (cycle-idx-table (make-fixnum-array (1+ $n)))
        (gcd-table (make-fixnum-array 10201))
        (fact-table (make-fixnum-array (1+ $n)))
        (w-table (make-fixnum-array 1030301))
        (m-table (make-fixnum-array 10201)))
    
    ;; 1. tau, inv-tau mapping
    (prog ((i 1) (val 0))
loop-i
          (if (> i $n) (go end))
          (setf val (1+ (mod (* $mod i) $n)))
          (setf (aref tau-table i) val)
          (setf (aref inv-tau-table val) i)
          (incf i)
          (go loop-i)
end
          )
     
    ;; 2. cycle-id, cycle-idx formulation
    (prog ((x 1) (k 1) (j 0))
loop-k
          (if (> k $m) (go end))
          (setf j 0)
loop-j
          (if (>= j k) (progn (incf k) (go loop-k)))
          (setf (aref cycle-id-table x) k)
          (setf (aref cycle-idx-table x) j)
          (incf x)
          (incf j)
          (go loop-j)
end
          )
     
    ;; 3. Precompute gcd-table
    (prog ((a 1) (b 1))
loop-a
          (if (> a $m) (go end-a))
          (setf b 1)
loop-b
          (when (> b $m) (incf a) (go loop-a))
          (setf (aref gcd-table (idx-m a b)) (compute-gcd a b))
          (incf b)
          (go loop-b)
end-a
          )
     
    ;; 4. Precompute factorials modulo 10^9+7
    (setf (aref fact-table 0) 1)
    (prog ((i 1))
loop-i
          (if (> i $n) (go end))
          (setf (aref fact-table i) (mod (* (aref fact-table (1- i)) i) $mod))
          (incf i)
          (go loop-i)
end
          )
     
    ;; 5. Precompute cycle multiplier M[cu][cv] = m! / lcm(cu, cv)
    (prog ((cu 1) (cv 1) (lcm-val 0) (fact-m (aref fact-table $m)))
loop-cu
          (if (> cu $m) (go end-cu))
          (setf cv 1)
loop-cv
          (when (> cv $m) (incf cu) (go loop-cu))
          (setf lcm-val (/ (* cu cv) (aref gcd-table (idx-m cu cv))))
          (setf (aref m-table (idx-m cu cv))
                (mod (* fact-m (mod-inverse lcm-val)) $mod))
          (incf cv)
          (go loop-cv)
end-cu
          )
     
    ;; 6. Precompute collision counts W-table O(n^2)
    ;; W[cx][cy][d] = number of pairs (x, y) where tau^{-1}(x) < tau^{-1}(y)
    ;; and (idx-x - idx-y) mod gcd(cx, cy) == d
    (prog ((x 1) (y 1) (cx 0) (cy 0) (idx-x 0) (idx-y 0) (g 0) (d 0)
           (inv-x 0) (inv-y 0))
loop-x
          (if (> x $n) (go end-x))
          (setf cx (aref cycle-id-table x))
          (setf idx-x (aref cycle-idx-table x))
          (setf inv-x (aref inv-tau-table x))
          (setf y 1)
loop-y
          (when (> y $n) (incf x) (go loop-x))
          (setf inv-y (aref inv-tau-table y))
          (if (< inv-x inv-y)
              (progn
                (setf cy (aref cycle-id-table y))
                (setf idx-y (aref cycle-idx-table y))
                (setf g (aref gcd-table (idx-m cx cy)))
                (setf d (mod (- idx-x idx-y) g))
                (incf (aref w-table (idx-w cx cy d)))))
          (incf y)
          (go loop-y)
end-x
          )
     
    ;; 7. Main accumulation O(n^2 / 2)
    (prog ((total-sum 0)
           (i 1) (j 1) (v 0) (u 0) (cv 0) (cu 0) (idx-v 0) (idx-u 0) (g 0) (d 0)
           (sum-s 0) (m-val 0) (w-val 0))
loop-i
          (if (>= i $n) (go end-i))
          (setf v (aref tau-table i))
          (setf cv (aref cycle-id-table v))
          (setf idx-v (aref cycle-idx-table v))
          (setf sum-s 0)
          (setf j (1+ i))
loop-j
          (if (> j $n) (go next-i))
          (setf u (aref tau-table j))
          (setf cu (aref cycle-id-table u))
          (setf idx-u (aref cycle-idx-table u))
        
          ;; 鏡像の訂正: 常に cu (小さい値側) を第一引数、cv (大きい値側) を第二引数に渡す
          (setf g (aref gcd-table (idx-m cu cv)))
          (setf d (mod (- idx-u idx-v) g))
        
          (setf m-val (aref m-table (idx-m cu cv)))
          (setf w-val (aref w-table (idx-w cu cv d)))
        
          ;; オーバーフローの絶対的回避 (fixnum max > 2.5e16) と mod の削減
          (incf sum-s (* m-val w-val))
        
          (incf j)
          (go loop-j)
next-i
          (setf sum-s (mod sum-s $mod))
          (setf total-sum (mod (+ total-sum
                                  (mod (* (aref fact-table (- $n i)) sum-s)
                                       $mod))
                               $mod))
          (when (= (mod i 1000) 0)
            (format t "Debug: Processed i=~A~%" i))
          (incf i)
          (go loop-i)
          end-i)))

#+| Do it | (project-euler-0902:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Debug: Processed i=1000
Debug: Processed i=2000
Debug: Processed i=3000
Debug: Processed i=4000
Debug: Processed i=5000
P(100) = 343557869

User time    =        1.229
System time  =        0.018
Elapsed time =        1.188
Allocation   = 8749504 bytes
1582 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 343557869
:ok