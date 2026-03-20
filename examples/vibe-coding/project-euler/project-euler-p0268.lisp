;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0268 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0268)

(defun solve (&optional (limit 9999999999999999))
  "10^16 未満 (デフォルト: 10^16 - 1) で条件を満たす数をカウントする"
  (declare (type (unsigned-byte 64) limit))
  (format t "Calculating for numbers up to ~A...~%" limit)
  
  (let* ((primes #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))
         (c-array (make-array 30 :element-type 'fixnum :initial-element 0))
         (ans 0))
    (declare (type integer ans)) ;; 負の加算が発生するため signed-integer を使用
    
    ;; 1. 一般化包除原理の係数 C_i = (-1)^(i-4) * Binomial(i-1, 3) を事前計算
    (iterate (for i from 4 to 25)
      (let ((val (truncate (* (- i 1) (- i 2) (- i 3)) 6)))
        (setf (aref c-array i) (if (evenp (- i 4)) val (- val)))))
        
    ;; 2. DFSによる素数部分集合の探索と枝刈り
    (labels ((dfs (idx count current-prod)
               (declare (type fixnum idx count)
                        (type (unsigned-byte 64) current-prod))
               
               ;; 4つ以上の素数を選んだ場合、その積に対する倍数の個数に係数を掛けて加算
               (when (>= count 4)
                 (incf ans (* (aref c-array count) (truncate limit current-prod))))
                 
               ;; 次の素数を掛けて探索を継続
               (iterate (for i from idx below 25)
                 (let ((next-prod (* current-prod (aref primes i))))
                   (declare (type (unsigned-byte 64) next-prod))
                   
                   ;; 素数は昇順なので、ここで limit を超えたら以降の素数も全て超える
                   (if (<= next-prod limit)
                       (dfs (1+ i) (1+ count) next-prod)
                       (finish))))))
                       
      (dfs 0 0 1)
      
      (format t "Final ans = ~A~%" ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating for numbers up to 9999999999999999...
Final ans = 785478606870985

User time    =        1.077
System time  =        0.017
Elapsed time =        1.030
Allocation   = 85672 bytes
375 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 785478606870985
:ok