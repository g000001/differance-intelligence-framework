;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0944 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0944)

(deftype  uint8 () '(unsigned-byte  8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))
(deftype uint60 () '(unsigned-byte 60))

(defun make-uint8-array (size &key (initialize-element 0))
  (make-array size :element-type 'uint8 :initial-element initialize-element))

(defun make-uint16-array (size &key (initialize-element 0))
  (make-array size :element-type 'uint16 :initial-element initialize-element))

(defun make-uint32-array (size &key (initialize-element 0))
  (make-array size :element-type 'uint32 :initial-element initialize-element))

(defun make-fixnum-array (size &key (initialize-element 0))
  (make-array size :element-type 'fixnum :initial-element initialize-element))

(defconstant $const-mod 1234567891)
(defconstant $const-block-size 65536)

(defun build-pow2-tables ()
  (let ((table-low (make-fixnum-array $const-block-size))
        (table-mid (make-fixnum-array $const-block-size))
        (table-high (make-fixnum-array $const-block-size)))
    (setf (aref table-low 0) 1)
    (iterate
      (for index from 1 below $const-block-size)
      (setf (aref table-low index) (mod (* (aref table-low (1- index)) 2) $const-mod)))
    
    (let ((step-mid (aref table-low (1- $const-block-size))))
      (setf step-mid (mod (* step-mid 2) $const-mod))
      (setf (aref table-mid 0) 1)
      (iterate
        (for index from 1 below $const-block-size)
        (setf (aref table-mid index) (mod (* (aref table-mid (1- index)) step-mid) $const-mod)))
      
      (let ((step-high (aref table-mid (1- $const-block-size))))
        (setf step-high (mod (* step-high step-mid) $const-mod))
        (setf (aref table-high 0) 1)
        (iterate
          (for index from 1 below $const-block-size)
          (setf (aref table-high index) (mod (* (aref table-high (1- index)) step-high) $const-mod)))))
    (values table-low table-mid table-high)))

(defun fast-pow2 (exponent table-low table-mid table-high)
  (let* ((part0 (logand exponent #xFFFF))
         (shifted1 (ash exponent -16))
         (part1 (logand shifted1 #xFFFF))
         (shifted2 (ash shifted1 -16))
         (part2 shifted2))
    (mod (* (aref table-low part0)
            (mod (* (aref table-mid part1)
                    (aref table-high part2))
                 $const-mod))
         $const-mod)))

(defun solve (&optional (limit-n #.(expt 10 14)))
  (multiple-value-bind (table-low table-mid table-high) (build-pow2-tables)
    (let* ((ans 0)
           (limit-v (isqrt limit-n))
           (pow2-n-1 (fast-pow2 (1- limit-n) table-low table-mid table-high)))
      
      (format t "Starting Part 1 (Limit: ~A)...~%" limit-v)
      (iterate
        (for current-x from 1 to limit-v)
        (let* ((current-k (floor limit-n current-x))
               (pow2-n-k (fast-pow2 (- limit-n current-k) table-low table-mid table-high))
               (term (mod (- pow2-n-1 pow2-n-k) $const-mod)))
          (when (< term 0) (incf term $const-mod))
          (setf ans (mod (+ ans (mod (* (mod current-x $const-mod) term) $const-mod)) $const-mod))
          (when (and (= (mod current-x 1000000) 0) (> current-x 0))
            (format t "Part 1 progress: ~A / ~A~%" current-x limit-v))))
      
      (let ((limit-max-k (floor limit-n (1+ limit-v))))
        (format t "Starting Part 2 (Max K: ~A)...~%" limit-max-k)
        (iterate
          (for current-k from 2 to limit-max-k)
          (let* ((right (floor limit-n current-k))
                 (left (1+ (floor limit-n (1+ current-k))))
                 (left-clipped (max left (1+ limit-v))))
            (when (<= left-clipped right)
              (let* ((count (- (1+ right) left-clipped))
                     ;; countと和の偶奇性を利用し、有理数(Ratio)アロケーションやモジュラ逆元を回避して整数除算を実行
                     (sum-x (if (evenp count)
                                (* (/ count 2) (+ left-clipped right))
                                (* count (/ (+ left-clipped right) 2))))
                     (sum-x-mod (mod sum-x $const-mod))
                     (pow2-n-k (fast-pow2 (- limit-n current-k) table-low table-mid table-high))
                     (term (mod (- pow2-n-1 pow2-n-k) $const-mod)))
                (when (< term 0) (incf term $const-mod))
                (setf ans (mod (+ ans (mod (* sum-x-mod term) $const-mod)) $const-mod))))
            (when (and (= (mod current-k 1000000) 0) (> current-k 0))
              (format t "Part 2 progress: ~A / ~A~%" current-k limit-max-k)))))
      
      (format t "Finished. Answer: ~A~%" ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Part 1 (Limit: 10000000)...
Part 1 progress: 1000000 / 10000000
Part 1 progress: 2000000 / 10000000
Part 1 progress: 3000000 / 10000000
Part 1 progress: 4000000 / 10000000
Part 1 progress: 5000000 / 10000000
Part 1 progress: 6000000 / 10000000
Part 1 progress: 7000000 / 10000000
Part 1 progress: 8000000 / 10000000
Part 1 progress: 9000000 / 10000000
Part 1 progress: 10000000 / 10000000
Starting Part 2 (Max K: 9999999)...
Part 2 progress: 1000000 / 9999999
Part 2 progress: 2000000 / 9999999
Part 2 progress: 3000000 / 9999999
Part 2 progress: 4000000 / 9999999
Part 2 progress: 5000000 / 9999999
Part 2 progress: 6000000 / 9999999
Part 2 progress: 7000000 / 9999999
Part 2 progress: 8000000 / 9999999
Part 2 progress: 9000000 / 9999999
Finished. Answer: 1228599511

User time    =        3.188
System time  =        0.042
Elapsed time =        3.148
Allocation   = 13740872 bytes
4297 Page faults
GC time      =        0.002
 |------------------------------------------------------------|#
;;→ 1228599511
:ok