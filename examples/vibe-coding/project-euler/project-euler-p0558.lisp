;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0558 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0558)

#||
(cl:comment "Project Euler 558: Irrational base representations")
(cl:comment "Previous scaled-and-rounded Bignum approaches cause infinite loops")
(cl:comment "because the residual never reaches EXACTLY zero due to approximation errors.")
(cl:comment "By leveraging the exact minimal polynomial x^3 = x^2 + 1, we map the real numbers")
(cl:comment "to an EXACT isomorphic integer domain. Let R = P/Q be a highly precise rational approximation of r.")
(cl:comment "Then r^k = A_k + B_k r + C_k r^2. We scale everything by Q^2.")
(cl:comment "V_k' = A_k Q^2 + B_k PQ + C_k P^2 becomes an EXACT integer.")
(cl:comment "The target X' = j^2 Q^2 is also an EXACT integer.")
(cl:comment "Since the approximation error is vastly smaller than the minimum gap of the algebraic integers,")
(cl:comment "the greedy algorithm on these integers is mathematically identical to the real ones,")
(cl:comment "and the residual will hit EXACTLY 0, guaranteeing termination without epsilons.")
||#

(defun get-r-rational ()
  "Calculates a highly precise rational approximation of r using 7 Newton iterations."
  (let ((x 3/2))
    (dotimes (i 7)
      (let* ((x2 (* x x))
             (x3 (* x x x)))
        (setq x (- x (/ (- x3 x2 1)
                        (- (* 3 x2) (* 2 x)))))))
    x))

(defconstant +min-k+ -500)
(defconstant +max-k+ 90)
(defconstant +k-size+ (+ +max-k+ (- +min-k+) 1))
(defconstant +offset+ (- +min-k+))

(defun solve ()
  (let* ((r-rat (get-r-rational))
         (P (numerator r-rat))
         (Q (denominator r-rat))
         (Q2 (* Q Q))
         (PQ (* P Q))
         (P2 (* P P))
         (Vk-arr (make-array +k-size+))
         (Ak-arr (make-array +k-size+ :element-type 'integer))
         (Bk-arr (make-array +k-size+ :element-type 'integer))
         (Ck-arr (make-array +k-size+ :element-type 'integer)))
    
    (setf (aref Ak-arr +offset+) 1)
    (setf (aref Bk-arr +offset+) 0)
    (setf (aref Ck-arr +offset+) 0)
    
    ;; k > 0 の漸化式
    (iterate (for k from 1 to +max-k+)
      (let ((idx (+ k +offset+))
            (prev (+ k -1 +offset+)))
        (setf (aref Ak-arr idx) (aref Ck-arr prev))
        (setf (aref Bk-arr idx) (aref Ak-arr prev))
        (setf (aref Ck-arr idx) (+ (aref Bk-arr prev) (aref Ck-arr prev)))))
        
    ;; k < 0 の漸化式
    (iterate (for k from -1 downto +min-k+)
      (let ((idx (+ k +offset+))
            (next (+ k 1 +offset+)))
        (setf (aref Ak-arr idx) (aref Bk-arr next))
        (setf (aref Bk-arr idx) (- (aref Ck-arr next) (aref Ak-arr next)))
        (setf (aref Ck-arr idx) (aref Ak-arr next))))
        
    ;; 誤差ゼロの完全なスケーリング整数 V_k' の生成
    (iterate (for i from 0 below +k-size+)
      (setf (aref Vk-arr i) (+ (* (aref Ak-arr i) Q2)
                               (* (aref Bk-arr i) PQ)
                               (* (aref Ck-arr i) P2))))
                               
    (format t "Precomputation done. Starting exact greedy calculation...~%")
    
    (let ((total-w 0)
          (last-idx +offset+)) ; k=0 から探索開始
      (iterate (for j from 1 to 5000000)
        (declare (type fixnum j))
        (let* ((j-sq (the fixnum (* j j)))
               (X-prime (* j-sq Q2))
               (w 0)
               (idx last-idx))
          
          ;; 直前の j で用いた最大の k を基準に上昇させ、対数計算を回避
          (iterate (while (<= (aref Vk-arr (1+ idx)) X-prime))
            (incf idx))
          (setf last-idx idx)
          
          ;; 誤差のない純粋な整数引き算による貪欲法
          (iterate (while (> X-prime 0))
            (iterate (while (> (aref Vk-arr idx) X-prime))
              (decf idx)
              (when (< idx 0)
                (error "Bounds exceeded: idx fell below 0 for j=~A. min-k is insufficient." j)))
            (setf X-prime (- X-prime (aref Vk-arr idx)))
            (incf w))
            
          (incf total-w w)
          (when (zerop (mod j 1000000))
            (format t "Processed ~A...~%" j))))
            
      (format t "Done.~%")
      total-w)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputation done. Starting exact greedy calculation...
Processed 1000000...
Processed 2000000...
Processed 3000000...
Processed 4000000...
Processed 5000000...
Done.

User time    =  0:04:22.521
System time  =        3.493
Elapsed time =  0:05:13.639
Allocation   = 194366898968 bytes
17744 Page faults
GC time      =        2.672
 |------------------------------------------------------------|#
;;→ 226754889
:ok