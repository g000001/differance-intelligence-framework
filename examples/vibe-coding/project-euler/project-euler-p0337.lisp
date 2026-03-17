;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0337 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0337)

#||
(cl:comment "Project Euler 337: Totient Stairstep Sequences")
(cl:comment "Constraints: a_1 = 6, phi(a_i) < phi(a_{i+1}) < a_i < a_{i+1}")
(cl:comment "Let dp[x] be the number of valid sequences ending in x.")
(cl:comment "Transition: dp[x] = sum(dp[y]) for y such that phi(y) < phi(x) < y < x.")
(cl:comment "Since we process x in increasing order, y < x is naturally satisfied for all processed y.")
(cl:comment "The remaining conditions are phi(y) < phi(x) and y > phi(x).")
(cl:comment "We can compute this as:")
(cl:comment "  Sum_{phi(y) < phi(x)} dp[y]  MINUS  Sum_{y <= phi(x) AND phi(y) < phi(x)} dp[y]")
(cl:comment "Crucial Invariant: Since phi(y) < y for all y >= 6, if y <= phi(x), then phi(y) < y <= phi(x) naturally implies phi(y) < phi(x).")
(cl:comment "Thus, the subtracted term simplifies perfectly to just the prefix sum: Sum_{y <= phi(x)} dp[y].")
(cl:comment "Algorithm: Use a Binary Indexed Tree (BIT) indexed by phi(y) to get the first sum in O(log N), and a simple array to get the prefix sum in O(1).")
||#

(declaim (ftype (function (fixnum) (simple-array (unsigned-byte 32) (*))) make-phi-array))
(defun make-phi-array (limit)
  (declare (type fixnum limit))
  (let ((phi-array (make-array (1+ limit) :element-type '(unsigned-byte 32))))
    (iterate (for index from 0 to limit)
      (setf (aref phi-array index) index))
    (iterate (for current-prime from 2 to limit)
      (declare (type fixnum current-prime))
      (when (= (aref phi-array current-prime) current-prime)
        (iterate (for multiple from current-prime to limit by current-prime)
          (declare (type fixnum multiple))
          (setf (aref phi-array multiple)
                (- (aref phi-array multiple)
                   (truncate (aref phi-array multiple) current-prime))))))
    phi-array))

(declaim (inline bit-add bit-query))

(defun bit-add (bit-tree index value)
  (declare (type (simple-array (unsigned-byte 32) (*)) bit-tree)
           (type fixnum index)
           (type (unsigned-byte 32) value))
  (let ((tree-size (1- (length bit-tree))))
    (declare (type fixnum tree-size))
    (iterate (while (<= index tree-size))
      ;; 除算(mod)を避け、加算と1回の引き算で高速化する
      (let ((new-value (+ (aref bit-tree index) value)))
        (declare (type (unsigned-byte 32) new-value))
        (if (>= new-value 100000000)
            (setf (aref bit-tree index) (- new-value 100000000))
            (setf (aref bit-tree index) new-value)))
      (setf index (+ index (logand index (- index)))))))

(defun bit-query (bit-tree index)
  (declare (type (simple-array (unsigned-byte 32) (*)) bit-tree)
           (type fixnum index))
  (let ((sum 0))
    (declare (type (unsigned-byte 32) sum))
    (iterate (while (> index 0))
      (let ((new-sum (+ sum (aref bit-tree index))))
        (declare (type (unsigned-byte 32) new-sum))
        (if (>= new-sum 100000000)
            (setf sum (- new-sum 100000000))
            (setf sum new-sum)))
      (setf index (- index (logand index (- index)))))
    sum))

(defun solve (&optional (limit 20000000))
  (declare (type fixnum limit))
  (let* ((phi-array (make-phi-array limit))
         (bit-tree (make-array (1+ limit) :element-type '(unsigned-byte 32) :initial-element 0))
         (sum-dp-array (make-array (1+ limit) :element-type '(unsigned-byte 32) :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 32) (*)) phi-array bit-tree sum-dp-array))
    
    (format t "phi array generated.~%")
    
    ;; 初期条件 x = 6
    (let ((initial-dp-value 1))
      (setf (aref sum-dp-array 6) initial-dp-value)
      (bit-add bit-tree (aref phi-array 6) initial-dp-value))
    
    (iterate (for current-x from 7 to limit)
      (declare (type fixnum current-x))
      (let* ((current-phi (aref phi-array current-x))
             ;; BITから phi(y) < phi(x) を満たす dp[y] の和を取得
             (sum-from-bit (bit-query bit-tree (1- current-phi)))
             ;; 累積和配列から y <= phi(x) を満たす dp[y] の和を取得し、O(1)で減算
             (sum-from-prefix (aref sum-dp-array current-phi))
             (current-dp (- sum-from-bit sum-from-prefix)))
        (declare (type fixnum current-phi)
                 (type (unsigned-byte 32) sum-from-bit sum-from-prefix)
                 (type fixnum current-dp))
        
        ;; 負の剰余系の補正
        (when (< current-dp 0)
          (setf current-dp (+ current-dp 100000000)))
        (when (>= current-dp 100000000)
          (setf current-dp (- current-dp 100000000)))
        
        ;; 累積和の更新
        (let ((new-sum (+ (aref sum-dp-array (1- current-x)) current-dp)))
          (declare (type (unsigned-byte 32) new-sum))
          (if (>= new-sum 100000000)
              (setf (aref sum-dp-array current-x) (- new-sum 100000000))
              (setf (aref sum-dp-array current-x) new-sum)))
        
        ;; BITへの登録
        (bit-add bit-tree current-phi current-dp)
        
        (when (and (= (mod current-x 2000000) 0) (<= current-x limit))
          (format t "Processed ~A...~%" current-x))))
    
    (format t "Done.~%")
    ;; a_n <= N となる全ての有効な数列の数
    (aref sum-dp-array limit)))



#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
phi array generated.
Processed 2000000...
Processed 4000000...
Processed 6000000...
Processed 8000000...
Processed 10000000...
Processed 12000000...
Processed 14000000...
Processed 16000000...
Processed 18000000...
Processed 20000000...
Done.

User time    =       37.120
System time  =        0.363
Elapsed time =       37.060
Allocation   = 246684480 bytes
63850 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 85068035
:ok