;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0935 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0935)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)

(declaim (inline count-coprime))
(defun count-coprime (x q spf primes-buf)
  "x 以下の整数の中で、q と互いに素なものの個数を包除原理で求める"
  (declare (type fixnum x q)
           (type (simple-array (unsigned-byte 32) (*)) spf primes-buf))
  (let ((k 0)
        (temp q))
    (declare (type fixnum k temp))
    ;; 高速素因数分解 (O(log q))
    (iterate (while (> temp 1))
      (let ((p (aref spf temp)))
        (declare (type fixnum p))
        (setf (aref primes-buf k) p)
        (incf k)
        (iterate (while (= (aref spf temp) p))
          (setf temp (truncate temp p)))))
    
    (let ((ans 0))
      (declare (type fixnum ans))
      ;; 包除原理によるビットマスク計算
      (iterate (for mask from 0 below (ash 1 k))
        (declare (type fixnum mask))
        (let ((prod 1)
              (signs 1))
          (declare (type fixnum prod signs))
          (iterate (for i from 0 below k)
            (declare (type fixnum i))
            (when (logbitp i mask)
              (setf prod (* prod (aref primes-buf i)))
              (setf signs (- signs))))
          (incf ans (* signs (truncate x prod)))))
      ans)))

(defun solve (&optional (n 100000000))
  (let* ((spf (make-array (1+ n) :element-type '(unsigned-byte 32)))
         (primes-buf (make-array 20 :element-type '(unsigned-byte 32)))
         (ans 0))
    (declare (type fixnum n)
             (type (unsigned-byte 64) ans) ; 10^16クラスの答えを安全に保持
             (type (simple-array (unsigned-byte 32) (*)) spf primes-buf))
    
    (format t "Phase 1: Initializing SPF array (~A MB)...~%" 
            (truncate (* 4 n) (* 1024 1024)))
    
    ;; エラトステネスの篩による SPF の構築 O(N log log N)
    (iterate (for i from 2 to n)
      (setf (aref spf i) i))
    (iterate (for i from 2 to (isqrt n))
      (declare (type fixnum i))
      (when (= (aref spf i) i)
        (iterate (for j from (* i i) to n by i)
          (declare (type fixnum j))
          (when (= (aref spf j) j)
            (setf (aref spf j) i)))))
            
    (format t "Phase 2: Calculating Coprimes over Farey Sequence...~%")
    
    ;; メインループ O(N)
    (iterate (for q from 1 to n)
      (declare (type fixnum q))
      (let* ((rem (logand q 3))
             ;; M_q = 4 / gcd(q, 4)
             (mq (cond ((= rem 0) 1)
                       ((= rem 2) 2)
                       (t 4)))
             ;; U_q = floor(N / M_q) + 1 - q
             (uq (- (+ (truncate n mq) 1) q)))
        (declare (type fixnum rem mq uq))
        
        (when (>= uq 1)
          (let ((c (count-coprime uq q spf primes-buf)))
            (declare (type fixnum c))
            (incf ans c)))
            
        (when (= (mod q 10000000) 0)
           (format t "  Progress: ~A / ~A~%" q n))))
           
    (format t "Result: F(~A) = ~A~%" n ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Phase 1: Initializing SPF array (381 MB)...
Phase 2: Calculating Coprimes over Farey Sequence...
  Progress: 10000000 / 100000000
  Progress: 20000000 / 100000000
  Progress: 30000000 / 100000000
  Progress: 40000000 / 100000000
  Progress: 50000000 / 100000000
  Progress: 60000000 / 100000000
  Progress: 70000000 / 100000000
  Progress: 80000000 / 100000000
  Progress: 90000000 / 100000000
  Progress: 100000000 / 100000000
Result: F(100000000) = 759908921637225

User time    =       25.013
System time  =        0.298
Elapsed time =       25.254
Allocation   = 400316688 bytes
97997 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 759908921637225
:ok