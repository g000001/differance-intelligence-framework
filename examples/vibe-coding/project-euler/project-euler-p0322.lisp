;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0322 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0322)

#||
(cl-text https://projecteuler.net/problem=322
(cl-comment "Project Euler 322: Binomial coefficients divisible by 10")
(cl-comment "Find T(m, n) = count of i in [n, m-1] such that C(i,n) = 0 mod 10.")
(cl-comment "By Lucas' Theorem, C(i,n) != 0 mod p iff i >=_p n.")
(cl-comment "Let A_p be the set of i < m such that C(i,n) != 0 mod p.")
(cl-comment "We need (m - n) - |A_2 union A_5| = (m - n) - (|A_2| + |A_5| - |A_2 cap A_5|).")
(cl-comment "Using digital DP and exploiting specific bounds (m=10^18, n=10^12-10).")
(cl-comment "Base 2 and Base 5 constraints decouple beautifully into a 1D range counting problem.")
(forall (i m n)
        (implies (and (>= i n) (< i m))
                 (iff (DivisibleBy (Binomial i n) 10)
                      (not (or (Submask n i 2)
                               (Submask n i 5))))))
)
||#

(defun mul-mod-2^40 (a b)
  "Bignumアロケーションを避けるための61bit Fixnum内での乗算とモジュロ"
  (declare (type fixnum a b))
  (let* ((a0 (logand a #xFFFFF))
         (a1 (ash a -20))
         (b0 (logand b #xFFFFF))
         (b1 (ash b -20))
         (c0 (* a0 b0))
         (c1 (logand (+ (* a0 b1) (* a1 b0)) #xFFFFF)))
    (logand (+ c0 (ash c1 20)) #xFFFFFFFFFF)))

(defun mod-inverse-2^40 (a)
  "ニュートン法による mod 2^40 のモジュラ逆数（奇数のみ）"
  (declare (type fixnum a))
  (let ((x 1)
        (mask #xFFFFFFFFFF))
    (declare (type fixnum x mask))
    (dotimes (i 6)
      (let ((ax (mul-mod-2^40 a x)))
        (setf x (mul-mod-2^40 x (logand (- 2 ax) mask)))))
    x))

(defun generate-submasks (k-mask)
  "Kの部分マスクを全て生成"
  (declare (type fixnum k-mask))
  (let* ((bits (make-array 40 :element-type 'fixnum))
         (bcnt 0))
    (declare (type fixnum bcnt))
    (dotimes (i 40)
      (when (/= 0 (logand k-mask (ash 1 i)))
        (setf (aref bits bcnt) i)
        (incf bcnt)))
    (let ((submasks (make-array (ash 1 bcnt) :element-type 'fixnum)))
      (dotimes (mask (ash 1 bcnt))
        (let ((val 0))
          (declare (type fixnum val))
          (dotimes (i bcnt)
            (when (/= 0 (logand mask (ash 1 i)))
              (setf val (logior val (ash 1 (aref bits i))))))
          (setf (aref submasks mask) val)))
      submasks)))

(defun count-valid-r (r-limit k-mask)
  "r < r-limit かつ r が k-mask の部分マスクであるものの個数"
  (declare (type fixnum r-limit k-mask))
  (let ((ans 0))
    (declare (type fixnum ans))
    (loop for i from 39 downto 0 do
      (let ((bit-r (logand (ash r-limit (- i)) 1))
            (bit-k (logand (ash k-mask (- i)) 1)))
        (when (= bit-r 1)
          (let ((zeros 0))
            (declare (type fixnum zeros))
            (loop for j from 0 below i do
              (when (= (logand (ash k-mask (- j)) 1) 1)
                (incf zeros)))
            (incf ans (ash 1 zeros)))
          (when (= bit-k 0)
            (return-from count-valid-r ans)))))
    ans))

(defun lower-bound (arr val)
  (declare (type (simple-array fixnum (*)) arr)
           (type fixnum val))
  (let ((low 0)
        (high (length arr)))
    (declare (type fixnum low high))
    (loop while (< low high) do
      (let ((mid (ash (+ low high) -1)))
        (declare (type fixnum mid))
        (if (< (aref arr mid) val)
            (setf low (1+ mid))
            (setf high mid))))
    low))

(defun upper-bound (arr val)
  (declare (type (simple-array fixnum (*)) arr)
           (type fixnum val))
  (let ((low 0)
        (high (length arr)))
    (declare (type fixnum low high))
    (loop while (< low high) do
      (let ((mid (ash (+ low high) -1)))
        (declare (type fixnum mid))
        (if (<= (aref arr mid) val)
            (setf low (1+ mid))
            (setf high mid))))
    low))

(defun count-in-range (x-arr y r-len)
  "巡回区間 [Y, Y + R] mod 2^40 に含まれる要素数をカウント"
  (declare (type (simple-array fixnum (*)) x-arr)
           (type fixnum y r-len))
  (let ((end (+ y r-len))
        (mask #xFFFFFFFFFF))
    (declare (type fixnum end mask))
    (if (< end (ash 1 40))
        (- (upper-bound x-arr end) (lower-bound x-arr y))
        (+ (- (length x-arr) (lower-bound x-arr y))
           (upper-bound x-arr (logand end mask))))))

(defun generate-b ()
  "Base 5における制約を満たす B の集合を再帰生成"
  (let ((n-bar #(4 1 0 0 0 0 0 0 0 0 0 0 4 0 1 2 3 3))
        (b-list (make-array 4800 :element-type 'fixnum))
        (idx 0))
    (declare (type fixnum idx))
    (labels ((dfs (j current-val weight)
               (declare (type fixnum j current-val weight))
               (if (= j 18)
                   (progn
                     (setf (aref b-list idx) current-val)
                     (incf idx))
                   (dotimes (d (1+ (aref n-bar j)))
                     (dfs (1+ j) (+ current-val (* d weight)) (* weight 5))))))
      (dfs 0 0 1)
      b-list)))

(defun solve ()
  (let* ((m 1000000000000000000)
         (n (- 1000000000000 10))
         (L (- m n))
         (K (logand (lognot n) #xFFFFFFFFFF))
         (submasks (generate-submasks K))
         (D 3814697265625) ; 5^18
         (E (mod-inverse-2^40 D))
         (num-submasks (length submasks))
         (x-arr (make-array num-submasks :element-type 'fixnum)))
    
    (declare (type fixnum m n L K D E num-submasks))

    (format t "Step 1: Generated ~A submasks.~%" num-submasks)
    
    ;; X_S = S * E mod 2^40 を事前計算してソート
    (dotimes (i num-submasks)
      (setf (aref x-arr i) (mul-mod-2^40 (aref submasks i) E)))
    (sort x-arr #'<)
    
    (let ((b-list (generate-b))
          (int-a2-a5 0)
          (range-len (1- (ash 1 18))))
      (declare (type fixnum int-a2-a5 range-len))
      
      (format t "Step 2: Generated ~A valid B elements.~%" (length b-list))
      
      ;; 1D Range Counting を用いた A2 ∩ A5 の交差計算
      (dotimes (i (length b-list))
        (let* ((b (aref b-list i))
               (y (mul-mod-2^40 b E)))
          (incf int-a2-a5 (count-in-range x-arr y range-len))))
      
      (format t "Step 3: Intersection A2 and A5 computed: ~A~%" int-a2-a5)
      
      (let* ((A5 (* 262144 4800))
             (Q2 (ash L -40))
             (R2 (logand L #xFFFFFFFFFF))
             (A2 (+ (* Q2 num-submasks) (count-valid-r R2 K)))
             (ans (+ (- L A2 A5) int-a2-a5)))
        
        (format t "Step 4: Final calculation.~%")
        (format t "|A2| (div by 2): ~A~%" A2)
        (format t "|A5| (div by 5): ~A~%" A5)
        (format t "Intersection : ~A~%" int-a2-a5)
        (format t "Ans: ~A~%" ans)
        ans))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: Generated 262144 submasks.
Step 2: Generated 4800 valid B elements.
Step 3: Intersection A2 and A5 computed: 321
Step 4: Final calculation.
|A2| (div by 2): 238418395136
|A5| (div by 5): 1258291200
Intersection : 321
Ans: 999998760323313995

User time    =        0.473
System time  =        0.020
Elapsed time =        0.372
Allocation   = 6762016 bytes
4794 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 999998760323313995
:ok
