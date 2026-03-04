;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0167 (:use cl iterate))
(in-package #:project-euler-0167)

#||
(cl-text project-euler-167-acx
  (cl-comment "CLIF model for P167: Ulam Sequences")
  
  (Problem P167)
  (constraint_size P167 100000000000)
  
  (cl-comment "NMF Definition: Naive O(k) sequence generation")
  (Algorithm A_Naive_Generation)
  (complexity A_Naive_Generation O_N)
  (NMF A_Naive_Generation)
  (attached_to_conventional_truth A_Naive_Generation)
  
  (cl-comment "ACX Jump to LFSR Cycle Detection")
  (Algorithm A_LFSR_Period)
  (target_of J_LFSR A_LFSR_Period)
  (complexity A_LFSR_Period O_1) 
  (grounded_in_ultimate_truth A_LFSR_Period)
  
  (cl-comment "Axiomatic Grounding: Schmerl and Spiegel theorem for U(2, 2n+1)")
  (fully_consumes_exact_rules P167)
  (avoids_inductive_guessing A_LFSR_Period)
  (grounds_in_deductive_logic A_LFSR_Period)
)
||#


(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun solve-ulam (n k)
  (declare (type fixnum n)
           (type integer k))
  (let* ((m (+ (* 2 n) 2))
         (mask (1- (ash 1 m)))
         (initial-state 0)
         ;; LFSRの最大周期は 2^22 - 1 ≒ 4.19M なので500万を確保しGC負債を抑える
         (b-array (make-array 5000000 :element-type 'bit))
         (b-len 0))
    (declare (type fixnum m mask initial-state b-len)
             (type (simple-array bit (5000000)) b-array))
    
    ;; 初期状態 (n番目から 2n+1番目までの奇数が数列に含まれる) の定礎
    (iterate (for i from 0 below m)
      (declare (type fixnum i))
      (let ((b (if (and (>= i n) (<= i (+ (* 2 n) 1))) 1 0)))
        (declare (type bit b))
        (setf (aref b-array b-len) b)
        (incf b-len)
        (setf initial-state (logior initial-state (ash b (- m 1 i))))))
        
    (let ((state initial-state)
          (period 0))
      (declare (type fixnum state period))
      ;; LFSRによる自己検算と周期の探索
      (iterate (for i from m)
        (declare (type fixnum i))
        (let* ((bit-0 (logand state 1))
               (bit-m (logand (ash state (- 1 m)) 1))
               (next-bit (logxor bit-0 bit-m)))
          (declare (type bit bit-0 bit-m next-bit))
          (setf state (logior (logand (ash state 1) mask) next-bit))
          (if (= state initial-state)
              (progn
                (setf period (- (1+ i) m))
                (finish))
              (progn
                (setf (aref b-array b-len) next-bit)
                (incf b-len)))))
      
      ;; 周期内の1の数をカウントし、剰余から目的のインデックスを割り出す
      (let ((w 0)
            (ones (make-array period :element-type 'fixnum)))
        (declare (type fixnum w)
                 (type (simple-array fixnum (*)) ones))
        (iterate (for i from 0 below period)
          (declare (type fixnum i))
          (when (= (aref b-array i) 1)
            (setf (aref ones w) i)
            (incf w)))
        
        ;; 2つの偶数 (2, 4n+4) を除外するため k - 2 番目の奇数を探す
        (let* ((target (- k 2))
               (target-0 (1- target))
               (cycles (floor target-0 w))
               (rem (mod target-0 w))
               (idx (aref ones rem))
               (absolute-idx (+ (* cycles period) idx)))
          (declare (type integer target target-0 cycles rem absolute-idx))
          ;; 奇数のインデックス i を実際の数値 2i+1 に復元
          (1+ (* 2 absolute-idx)))))))

(defun solve-p167 ()
  (let ((k 100000000000)
        (total 0))
    (iterate (for n from 2 to 10)
      (incf total (solve-ulam n k)))
    total))


#+| Do it | (solve-p167 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-p167)

User time    =        0.398
System time  =        0.012
Elapsed time =        0.366
Allocation   = 48289448 bytes
2649 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 3916160068885
:ok