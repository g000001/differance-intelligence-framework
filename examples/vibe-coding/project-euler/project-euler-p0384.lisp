;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0384 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0384)

(defun solve (&optional (n 45))
  ;; Precompute the absolute weight of each bit (A_j = 2^(floor((j+1)/2)))
  ;; and the maximum possible sum of weights for remaining lower bits (max-rem)
  (let* ((a-arr (make-array 65 :element-type 'fixnum))
         (max-rem (make-array 65 :element-type 'fixnum)))
    (iterate (for j from 0 to 62)
      (setf (aref a-arr j) (ash 1 (floor (+ j 1) 2)))
      (if (= j 0)
          (setf (aref max-rem j) (aref a-arr j))
          (setf (aref max-rem j) (+ (aref max-rem (1- j)) (aref a-arr j)))))
          
    ;; Memoization table. State space is heavily pruned and extremely narrow.
    (let ((memo (make-hash-table :test 'eql :size 2000000))
          (offset 5000000000)) ; Offset to handle negative target-s safely within fixnum bounds
      (labels ((count-n (j prev par target-s)
                 (declare (type fixnum j prev par target-s))
                 ;; PERFECT PRUNING: If target is mathematically unreachable, return 0 immediately.
                 (let ((limit (if (< j 0) 0 (aref max-rem j))))
                   (declare (type fixnum limit))
                   (if (> (abs target-s) (+ limit 1))
                       0
                       (if (< j 0)
                           (if (= target-s (if (= par 0) 1 -1)) 1 0)
                           ;; Pack (j, prev, par, target-s) into a 61-bit fixnum
                           (let ((key (logior j (ash prev 6) (ash par 7) (ash (+ target-s offset) 8))))
                             (multiple-value-bind (val win) (gethash key memo)
                               (if win val
                                   (let* ((res0 (count-n (- j 1) 0 par target-s))
                                          (new-par (logxor par prev))
                                          (weight (if (= par 0) (aref a-arr j) (- (aref a-arr j))))
                                          (res1 (count-n (- j 1) 1 new-par (- target-s weight)))
                                          (ans (+ res0 res1)))
                                     (setf (gethash key memo) ans)
                                     ans))))))))
               
               ;; Trace down the DP tree to find the exact n-th number
               (find-nth (j prev par target-s c)
                 (declare (type fixnum j prev par target-s)
                          (type integer c))
                 (if (< j 0)
                     0
                     (let ((cnt0 (count-n (- j 1) 0 par target-s)))
                       (if (<= c cnt0)
                           ;; Go down the 0-branch
                           (find-nth (- j 1) 0 par target-s c)
                           ;; Go down the 1-branch
                           (let* ((new-par (logxor par prev))
                                  (weight (if (= par 0) (aref a-arr j) (- (aref a-arr j))))
                                  (ans1 (find-nth (- j 1) 1 new-par (- target-s weight) (- c cnt0))))
                             (logior (ash 1 j) ans1)))))))
        
        ;; Standard Fibonacci sequence
        (let ((f (make-array 46 :element-type 'fixnum)))
          (setf (aref f 0) 1
                (aref f 1) 1)
          (iterate (for i from 2 to 45)
            (setf (aref f i) (+ (aref f (1- i)) (aref f (- i 2)))))
            
          (format t "Calculating GF(t) for t = 2 to ~A using Perfectly Pruned DP...~%" n)
          (let ((sum 0))
            (iterate (for t-val from 2 to n)
              (let* ((ft (aref f t-val))
                     (ft-1 (aref f (1- t-val)))
                     ;; Start from 62nd bit (sufficient to reach F(45) ~ 1.13*10^9)
                     (gf (find-nth 62 0 0 ft ft-1)))
                (incf sum gf)
                (when (= (mod t-val 10) 0)
                  (format t "Computed GF(~A) = ~A~%" t-val gf))))
            (format t "Final sum = ~A~%" sum)
            sum))))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating GF(t) for t = 2 to 45 using Perfectly Pruned DP...
Computed GF(10) = 4458
Computed GF(20) = 71020409
Computed GF(30) = 1155709295450
Computed GF(40) = 17638305948120780
Final sum = 3354706415856332783

User time    =        0.029
System time  =        0.002
Elapsed time =        0.024
Allocation   = 16704472 bytes
143 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 3354706415856332783
:ok