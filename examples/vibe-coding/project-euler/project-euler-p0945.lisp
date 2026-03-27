;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0945 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0945)

(deftype uint32 () '(unsigned-byte 32))

(defun make-uint32-array (size &key (initialize-element 0))
  (make-array size :element-type 'uint32 :initial-element initialize-element))


(defun tz (n-val)
  (declare (type fixnum n-val))
  (if (= n-val 0)
      0
      (1- (integer-length (logand n-val (- n-val))))))

(defun inflate (u-val)
  ;; F_2[x]における多項式の二乗は、整数のビット表現の間に0を挿入したものになる
  (declare (type fixnum u-val))
  (let ((res 0))
    (declare (type fixnum res))
    (iterate (for bit-index from 0 below 12)
      (when (logbitp bit-index u-val)
        (setf res (logior res (ash 1 (* 2 bit-index))))))
    res))

(defun xor-mul (a-val b-val)
  ;; F_2[x]における多項式乗算 (キャリーなし乗算)
  (declare (type fixnum a-val b-val))
  (let ((res 0)
        (len (integer-length a-val)))
    (declare (type fixnum res len))
    (iterate (for bit-index from 0 below len)
      (when (logbitp bit-index a-val)
        (setf res (logxor res (ash b-val bit-index)))))
    res))

(defun solve (&optional (limit-n #.(expt 10 7)))
  ;; N=10^7の場合、奇数のみ管理すれば配列サイズを半減(キャッシュ効率2倍)できる
  (let* ((half-size (ash (1+ limit-n) -1))
         (core-array (make-uint32-array half-size))
         (count-even (make-uint32-array half-size))
         (count-odd (make-uint32-array half-size))
         (max-deg-n (1- (integer-length limit-n))))
    
    (format t "Initializing core array...~%")
    (iterate (for current-i from 1 to limit-n by 2)
      (setf (aref core-array (ash current-i -1)) current-i))
    
    (format t "Sieving squares (Polynomial DP)...~%")
    (iterate (for current-v from 1 to limit-n by 2)
      (when (= (aref core-array (ash current-v -1)) current-v)
        ;; Vがスクエアフリーである場合のみ、そのVをベースに平方U^2を伝播させる
        (let* ((deg-v (1- (integer-length current-v)))
               (max-deg-u (floor (- max-deg-n deg-v) 2))
               (limit-u (1- (ash 1 (1+ max-deg-u)))))
          (iterate (for current-u from 3 to limit-u by 2)
            (let* ((u-sq (inflate current-u))
                   (current-w (xor-mul current-v u-sq)))
              (when (<= current-w limit-n)
                (setf (aref core-array (ash current-w -1)) current-v)))))))
    
    (format t "Counting parity of trailing zeros per core...~%")
    (iterate (for current-n from 1 to limit-n)
      (let* ((trailing-zeros (tz current-n))
             (n-prime (ash current-n (- trailing-zeros)))
             (core-idx (ash (aref core-array (ash n-prime -1)) -1)))
        (if (evenp trailing-zeros)
            (incf (aref count-even core-idx))
            (incf (aref count-odd core-idx)))))
    
    (format t "Calculating symmetric combinations...~%")
    (let ((ans (1+ limit-n))) ; a=0の場合の解 (0, b, b) は N+1 個
      (declare (type fixnum ans))
      (iterate (for idx from 0 below half-size)
        (let ((even-cnt (aref count-even idx))
              (odd-cnt (aref count-odd idx)))
          (when (and (> even-cnt 0) (> odd-cnt 0))
            (incf ans (* even-cnt odd-cnt)))))
      
      (format t "Finished. Answer: ~A~%" ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing core array...
Sieving squares (Polynomial DP)...
Counting parity of trailing zeros per core...
Calculating symmetric combinations...
Finished. Answer: 83357132

User time    =        2.142
System time  =        0.045
Elapsed time =        2.129
Allocation   = 60223048 bytes
23707 Page faults
GC time      =        0.020
 |------------------------------------------------------------|#
;;→ 83357132
:ok