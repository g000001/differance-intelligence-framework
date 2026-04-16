;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0636 (:use cl iterate) (:export #:solve))
(in-package #:project-euler-0636)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(defun factorial (num-val)
  (case num-val
    (0 1) (1 1) (2 2) (3 6) (4 24) (5 120)
    (6 720) (7 5040) (8 40320) (9 362880) (10 3628800)
    (otherwise 0)))

(defun power (base-val exp-val mod-val)
  (let ((result-val 1)
        (curr-base (mod base-val mod-val))
        (curr-exp exp-val))
    (iterate (while (> curr-exp 0))
             (when (oddp curr-exp)
               (setf result-val (mod (* result-val curr-base) mod-val)))
             (setf curr-base (mod (* curr-base curr-base) mod-val))
             (setf curr-exp (ash curr-exp -1)))
    result-val))

(defun extended-gcd (num-a num-b)
  (if (zerop num-a)
      (values num-b 0 1)
      (multiple-value-bind (gcd-val x-val y-val) (extended-gcd (mod num-b num-a) num-a)
        (values gcd-val
                (- y-val (* (floor num-b num-a) x-val))
                x-val))))

(defun mod-inverse (num-a mod-val)
  (multiple-value-bind (gcd-val x-val y-val) (extended-gcd num-a mod-val)
    (declare (ignore y-val))
    (if (= gcd-val 1)
        (mod x-val mod-val)
        (error "Modular inverse does not exist for ~A mod ~A" num-a mod-val))))

(defun get-exponents (limit-n)
  (let ((primes-array (make-array (1+ limit-n) :element-type 'bit :initial-element 1))
        (exp-counts-hash (make-hash-table)))
    (setf (bit primes-array 0) 0 (bit primes-array 1) 0)
    (iterate (for prime-num from 2 to limit-n)
             (when (= (bit primes-array prime-num) 1)
               (let ((exp-count 0)
                     (curr-power prime-num))
                 (iterate (while (<= curr-power limit-n))
                          (incf exp-count (floor limit-n curr-power))
                          (if (> curr-power (floor limit-n prime-num))
                              (leave)
                              (setf curr-power (* curr-power prime-num))))
                 (incf (gethash exp-count exp-counts-hash 0))
                 (iterate (for inner-idx from (* prime-num prime-num) to limit-n by prime-num)
                          (setf (bit primes-array inner-idx) 0)))))
    exp-counts-hash))

(defun get-all-partitions-info (weights-array)
  (let* ((num-elements (length weights-array))
         (block-counts (make-array num-elements :initial-element 0))
         (block-sums (make-array num-elements :initial-element 0))
         (results-hash (make-hash-table :test 'equal)))
    (labels ((dfs (element-idx num-blocks)
               (if (= element-idx num-elements)
                   (let ((mobius-val 1)
                         (weight-multiset nil))
                     (iterate (for block-idx from 0 below num-blocks)
                              (let ((block-size (aref block-counts block-idx)))
                                (setf mobius-val (* mobius-val
                                                    (if (evenp (1- block-size)) 1 -1)
                                                    (factorial (1- block-size))))
                                (push (aref block-sums block-idx) weight-multiset)))
                     (setf weight-multiset (sort weight-multiset #'<))
                     (incf (gethash weight-multiset results-hash 0) mobius-val))
                   (iterate (for block-idx from 0 to num-blocks)
                            (let ((weight-val (aref weights-array element-idx)))
                              (incf (aref block-counts block-idx))
                              (incf (aref block-sums block-idx) weight-val)
                              (dfs (1+ element-idx)
                                   (if (= block-idx num-blocks)
                                       (1+ num-blocks)
                                       num-blocks))
                              (decf (aref block-sums block-idx) weight-val)
                              (decf (aref block-counts block-idx)))))))
      (dfs 0 0)
      results-hash)))

(defun solve ()
  (let* (($limit #.(expt 10 6))
         ($mod #.(+ (expt 10 9) 7))
         ($weights #(1 2 2 3 3 3 4 4 4 4))
         ($exp-counts (get-exponents $limit))
         ($max-exp 0))
    
    (iterate (for (exp-val exp-freq) in-hashtable $exp-counts)
             (setf $max-exp (max $max-exp exp-val)))
    
    (format t "Step 1: Extracted prime exponents. Max exponent = ~A~%" $max-exp)
    
    (let (($partitions-info (get-all-partitions-info $weights))
          ($dp-array (make-array (1+ $max-exp) :element-type '(unsigned-byte 64) :initial-element 0))
          ($total-ways 0)
          ($valid-multiset-count 0))
      
      (iterate (for (weight-multiset coef-val) in-hashtable $partitions-info)
               (when (not (zerop (mod coef-val $mod)))
                 (incf $valid-multiset-count)))
      
      (format t "Step 2: Generated partitions. Non-zero invariant classes = ~A~%" $valid-multiset-count)
      
      (iterate (for (weight-multiset coef-val) in-hashtable $partitions-info)
               (let ((modulo-coef (mod coef-val $mod)))
                 (when (not (zerop modulo-coef))
                   ;; DP初期化
                   (iterate (for dp-idx from 0 to $max-exp)
                            (setf (aref $dp-array dp-idx) 0))
                   (setf (aref $dp-array 0) 1)
                   
                   ;; DP推移：縮退した多重集合のみを使って計算
                   (iterate (for weight-val in weight-multiset)
                            (iterate (for dp-idx from weight-val to $max-exp)
                                     (let ((new-val (+ (aref $dp-array dp-idx)
                                                       (aref $dp-array (- dp-idx weight-val)))))
                                       (setf (aref $dp-array dp-idx)
                                             (if (>= new-val $mod)
                                                 (- new-val $mod)
                                                 new-val)))))
                   
                   (let ((multiset-ways 1))
                     (iterate (for (exp-val exp-freq) in-hashtable $exp-counts)
                              (setf multiset-ways
                                    (mod (* multiset-ways
                                            (power (aref $dp-array exp-val) exp-freq $mod))
                                         $mod)))
                     
                     (setf $total-ways (mod (+ $total-ways (* modulo-coef multiset-ways)) $mod))))))
      
      (format t "Step 3: DP aggregation complete. Raw sum = ~A~%" $total-ways)
      
      ;; 1! * 2! * 3! * 4! = 288 で軌道を割る
      (let (($div-factor 288))
        (setf $total-ways (mod (* $total-ways (mod-inverse $div-factor $mod)) $mod)))
      
      (format t "Final Result F(~A!): ~A~%" $limit $total-ways)
      $total-ways)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: Extracted prime exponents. Max exponent = 999993
Step 2: Generated partitions. Non-zero invariant classes = 966
Step 3: DP aggregation complete. Raw sum = 255835008
Final Result F(1000000!): 888316

User time    =       25.434
System time  =        0.226
Elapsed time =       25.685
Allocation   = 17501376 bytes
2013 Page faults
GC time      =        0.001
 |------------------------------------------------------------|#
;;→ 888316
:ok