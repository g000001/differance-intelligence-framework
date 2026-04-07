;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0536 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0536)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)

(defun generate-primes (limit)
  "指定された上限までの素数配列を生成する"
  (let ((sieve (make-array (1+ limit) :element-type 'bit :initial-element 0))
        (lst nil))
    (iterate (for i from 2 to limit)
      (when (zerop (sbit sieve i))
        (push i lst)
        (iterate (for j from (* i i) to limit by i)
          (setf (sbit sieve j) 1))))
    (coerce (nreverse lst) 'simple-vector)))

(defun filter-dfs-primes (all-primes max-val)
  "全素数からDFS用の初期素数（2を除く、max-val以下）を抽出する"
  (let ((lst nil))
    (iterate (for i from 1 below (length all-primes)) ; 2 をスキップ (m は必ず奇数)
      (let ((p (aref all-primes i)))
        (when (> p max-val) (finish))
        (push p lst)))
    (coerce (nreverse lst) 'simple-vector)))

(defun solve (&optional (limit (expt 10 12)))
  (format t "Phase 1: Initializing primes...~%")
  (let* ((all-primes (generate-primes (expt 10 6)))
         (num-all-primes (length all-primes))
         (dfs-primes (filter-dfs-primes all-primes 1000))
         (num-dfs-primes (length dfs-primes))
         (total-sum 3)) ; 1 と 2 は問題条件を満たす不変の特殊解
    
    (labels ((dfs (idx m L)
               (declare (type fixnum idx m L))
               (iterate (for i from idx below num-dfs-primes)
                 (let* ((p (aref dfs-primes i))
                        (new-m (* m p)))
                   (declare (type fixnum p new-m))
                   (when (> new-m limit) (return))
                   (let* ((p-1 (1- p))
                          (new-L (lcm L p-1)))
                     (declare (type fixnum p-1)
                              (type integer new-L))
                     (when (<= new-L (+ limit 3))
                       (let ((g (gcd new-m new-L)))
                         (when (or (= g 1) (= g 3))
                           (when (zerop (mod (+ new-m 3) new-L))
                             (incf total-sum new-m))
                           (dfs (1+ i) new-m new-L))))))))
             
             (check-k (k p m+3)
               (declare (type fixnum k p m+3))
               (let ((temp k))
                 (declare (type fixnum temp))
                 (iterate (for i from 1 below num-all-primes)
                   (let ((q (aref all-primes i)))
                     (declare (type fixnum q))
                     (when (> (* q q) temp) (finish))
                     (when (zerop (mod temp q))
                       (when (zerop (mod temp (* q q))) (return-from check-k nil))
                       (when (not (zerop (mod m+3 (1- q)))) (return-from check-k nil))
                       (setq temp (truncate temp q)))))
                 (when (> temp 1)
                   (when (>= temp p) (return-from check-k nil))
                   (when (not (zerop (mod m+3 (1- temp)))) (return-from check-k nil)))
                 t))

             (run-a-iteration ()
               (let ((sum 0))
                 (declare (type integer sum))
                 (iterate (for a from 1 to (expt 10 6))
                   (let ((max-p (+ (isqrt (truncate limit a)) 2)))
                     (declare (type fixnum max-p))
                     (iterate (for i from 0 below num-all-primes)
                       (let ((p (aref all-primes i)))
                         (declare (type fixnum p))
                         (when (<= p 1000) (next-iteration))
                         (when (> p max-p) (return))
                         (let ((m-val (- (* a p p) (* (+ a 3) p))))
                           (declare (type fixnum m-val))
                           (when (> m-val limit) (return))
                           (let ((k (- (* a (1- p)) 3)))
                             (declare (type fixnum k))
                             (when (> k 0)
                               (when (check-k k p (+ m-val 3))
                                 (incf sum m-val)))))))))
                 sum)))

      (format t "Phase 2: Executing optimized DFS for sub-1000 roots...~%")
      (dfs 0 1 1)
      
      (format t "Phase 3: Executing A-iteration for supra-1000 roots...~%")
      (incf total-sum (run-a-iteration))
      
      (format t "Target Initial Re-eval: S(10^6) = 22868117 ? (Expected for limit=10^6)~%")
      (format t "Result: S(~A) = ~A~%" limit total-sum)
      total-sum)))


#+| Do it | (solve )
#||
Phase 1: Initializing primes...
Phase 2: Executing optimized DFS for sub-1000 roots...
Phase 3: Executing A-iteration for supra-1000 roots...
Target Initial Re-eval: S(10^6) = 22868117 ? (Expected for limit=10^6)
Result: S(1000000000000) = 3557005261906288
Evaluation took:
  525.093 seconds of real time
  518.804860 seconds of total run time (515.634871 user, 3.169989 system)
  98.80% CPU
  1,676,146,985,391 processor cycles
  1,892,944 bytes consed
  
3557005261906288
||#

