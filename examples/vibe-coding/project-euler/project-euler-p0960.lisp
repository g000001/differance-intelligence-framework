;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0960 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0960)

(declaim (inline mod-pow))
(defun mod-pow (base exp mod-val)
  (declare (type integer base exp mod-val))
  (let ((res 1)
        (b (mod base mod-val)))
    (iterate (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (* res b) mod-val)))
      (setf b (mod (* b b) mod-val))
      (setf exp (ash exp -1)))
    res))

(defun solve (&optional (n 100))
  (format t "Starting Dimensional Collapse (Cayley's Tree Formula) for N=~A...~%" n)
  (let* ((m 1000000007)
         (fact (make-array (1+ n) :element-type 'integer :initial-element 1))
         (inv-fact (make-array (1+ n) :element-type 'integer :initial-element 1))
         (total-sum 0))
    
    ;; Precompute factorials
    (iterate (for i from 2 to n)
      (setf (aref fact i) (mod (* (aref fact (1- i)) i) m)))
    
    ;; Precompute inverse factorials (Fermat's Little Theorem)
    (setf (aref inv-fact n) (mod-pow (aref fact n) (- m 2) m))
    (iterate (for i from (1- n) downto 1)
      (setf (aref inv-fact i) (mod (* (aref inv-fact (1+ i)) (1+ i)) m)))
    
    (labels ((nCk (n k)
               (if (or (< k 0) (> k n))
                   0
                   (mod (* (aref fact n)
                           (mod (* (aref inv-fact k) (aref inv-fact (- n k))) m))
                        m))))
      
      (iterate (for k from 1 to (1- n))
        (let* ((min-val (min k (- n k)))
               (comb (nCk n k))
               ;; k^{k-1} および (n-k)^{n-k-1} の計算
               (p1 (mod-pow k (1- k) m))
               (p2 (mod-pow (- n k) (1- (- n k)) m))
               
               ;; 中間生成物のモジュラ計算（Bignum爆発の完全回避）
               (term (mod (* comb min-val) m))
               (term2 (mod (* term p1) m))
               (term3 (mod (* term2 p2) m)))
          (setf total-sum (mod (+ total-sum term3) m)))))
    
    ;; 最後に対称性の 1/2 (mod 10^9+7 での 2 の逆元 = 500000004) と (n-1)! を掛ける
    (let* ((ans (mod (* total-sum 500000004) m))
           (final-ans (mod (* ans (aref fact (1- n))) m)))
      (format t "Finished. Answer: ~A~%" final-ans)
      final-ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Dimensional Collapse (Cayley's Tree Formula) for N=100...
Finished. Answer: 243559751

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 1872 bytes
15 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 243559751
:ok