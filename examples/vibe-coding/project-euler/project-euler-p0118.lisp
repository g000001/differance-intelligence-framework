;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0118 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0118)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p t)


(defun prime-p (n)
  "6k +/- 1 最適化を用いた素数判定。"
  (declare (type fixnum n))
  (cond ((< n 2) nil)
        ((< n 4) t)
        ((zerop (logand n 1)) nil)
        ((zerop (mod n 3)) nil)
        (t (let ((limit (isqrt n)))
             (iterate (for i from 5 by 6)
                      (while (<= i limit))
                      (when (zerop (mod n i)) (return-from prime-p nil))
                      (let ((i+2 (+ i 2)))
                        (when (and (<= i+2 limit) (zerop (mod n i+2)))
                          (return-from prime-p nil)))
                      (finally (return t)))))))

(defun digits-to-int (digits)
  "数字リストを整数に変換。"
  (let ((n 0))
    (declare (type fixnum n))
    (dolist (d digits n)
      (setf n (+ (* n 10) (the fixnum d))))))

(defun get-digits-from-mask (mask)
  "ビットマスクから使用されている数字(1-9)のリストを生成。"
  (iterate (for i from 0 below 9)
           (when (logbitp i mask)
             (collect (1+ i)))))

(defun count-primes-for-mask (mask)
  "与えられた数字の集合（マスク）から作れる素数の個数をカウント。"
  (let* ((digits (get-digits-from-mask mask))
         (sum (reduce #'+ digits))
         (len (length digits)))
    ;; 3の倍数判定：桁の和が3の倍数かつ2桁以上なら素数は作れない
    (if (and (zerop (mod sum 3)) (> len 1))
        0
        (let ((count 0))
          (declare (type fixnum count))
          (map-permutations
           (lambda (p)
             ;; 素数の末尾桁フィルタリング (2, 5 以外は 1, 3, 7, 9 のみ)
             (let ((last (car (last p))))
               (when (or (member last '(1 3 7 9))
                         (and (= len 1) (member last '(2 5))))
                 (when (prime-p (digits-to-int p))
                   (incf count)))))
           digits)
          count))))

(defun count-sets-dfs (mask memo primes-per-mask)
  "LSB固定法を用いた重複のない集合分割の探索。"
  (declare (type fixnum mask)
           (type (simple-array fixnum (512)) memo primes-per-mask))
  (if (zerop mask) (return-from count-sets-dfs 1))
  (let ((cached (aref memo mask)))
    (if (>= cached 0) (return-from count-sets-dfs cached)))
  
  (let ((low-bit (iterate (for i from 0 below 9)
                          (when (logbitp i mask) (return i))))
        (total 0))
    (declare (type fixnum low-bit total))
    ;; 現在のマスクの部分集合を列挙
    (let ((submask mask))
      (iterate (while (> submask 0))
               ;; 対称性を破るため、常に最小の桁(low-bit)を含む部分集合のみを選択
               (when (logbitp low-bit submask)
                 (let ((p-count (aref primes-per-mask submask)))
                   (when (> p-count 0)
                     (incf total (* p-count (count-sets-dfs (logxor mask submask) memo primes-per-mask))))))
               (setf submask (logand (1- submask) mask))))
    (setf (aref memo mask) total)
    total))

(defun solve ()
  (let ((primes-per-mask (make-array 512 :element-type 'fixnum :initial-element 0))
        (memo (make-array 512 :element-type 'fixnum :initial-element -1))
        (all-digits-mask (1- (ash 1 9)))) ; 111111111 (binary) = 511
    
    (format t "Step 1: Precomputing primes for each digit subset...~%")
    (iterate (for mask from 1 below 512)
             (setf (aref primes-per-mask mask) (count-primes-for-mask mask))
             (when (zerop (mod mask 64))
               (format t "Processed ~D/511 masks...~%" mask)))
    
    (format t "Step 2: Counting distinct prime sets using DFS...~%")
    (let ((result (count-sets-dfs all-digits-mask memo primes-per-mask)))
      (format t "Final Result: ~A~%" result)
      result)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: Precomputing primes for each digit subset...
Processed 64/511 masks...
Processed 128/511 masks...
Processed 192/511 masks...
Processed 256/511 masks...
Processed 320/511 masks...
Processed 384/511 masks...
Processed 448/511 masks...
Step 2: Counting distinct prime sets using DFS...
Final Result: 44680

User time    =        0.353
System time  =        0.014
Elapsed time =        0.337
Allocation   = 49933712 bytes
1332 Page faults
GC time      =        0.003
 |------------------------------------------------------------|#
;;→ 44680
:ok