;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0245 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0245)

;; -----------------------------------------------------------------------------
;; ユーティリティと素数判定
;; -----------------------------------------------------------------------------
(defun make-primes (limit)
  (let ((sieve (make-array (1+ limit) :element-type 'bit :initial-element 0))
        (primes (make-array 10000 :element-type 'fixnum :fill-pointer 0 :adjustable t)))
    (iterate (for p from 2 to limit)
      (when (= (sbit sieve p) 0)
        (vector-push-extend p primes)
        (iterate (for i from (* p p) to limit by p)
          (setf (sbit sieve i) 1))))
    primes))

;; k^2 - k + 1 を割り切る可能性のある素数は p=3 または p ≡ 1 (mod 6) のみ
(defun make-test-primes (primes)
  (let ((t-primes (make-array 10000 :element-type 'fixnum :fill-pointer 0 :adjustable t)))
    (iterate (for i from 0 below (length primes))
      (let ((p (aref primes i)))
        (when (or (= p 3) (= (mod p 6) 1))
          (vector-push-extend p t-primes))))
    t-primes))

(defun expt-mod (base power divisor)
  (declare (type (unsigned-byte 64) base power divisor))
  (let ((res 1)
        (b (mod base divisor))
        (p power))
    (iterate (while (> p 0))
      (when (= (logand p 1) 1)
        (setf res (mod (* res b) divisor)))
      (setf b (mod (* b b) divisor))
      (setf p (ash p -1)))
    res))

(defun is-prime (n)
  (declare (type (unsigned-byte 64) n))
  (if (<= n 1) (return-from is-prime nil))
  (if (<= n 3) (return-from is-prime t))
  (if (evenp n) (return-from is-prime nil))
  (let ((d (1- n))
        (s 0))
    (iterate (while (evenp d))
      (setf d (ash d -1))
      (incf s))
    (iterate (for a in '(2 3 5 7 11 13 17))
      (if (>= a n) (return t))
      (let ((x (expt-mod a d n))
            (composite t))
        (if (or (= x 1) (= x (1- n)))
            (setf composite nil)
            (iterate (for r from 1 below s)
              (setf x (mod (* x x) n))
              (when (= x (1- n))
                (setf composite nil)
                (finish))))
        (when composite (return-from is-prime nil))))
    t))

;; -----------------------------------------------------------------------------
;; 試し割り法による素因数分解と約数列挙
;; -----------------------------------------------------------------------------
(defun factorize (v test-primes)
  (declare (type (unsigned-byte 64) v)
           (type (array fixnum (*)) test-primes))
  (let ((factors nil))
    (iterate (for i from 0 below (length test-primes))
      (let ((p (aref test-primes i)))
        (declare (type fixnum p))
        ;; v が素数になればそれ以上の試し割りは不要
        (if (> (* p p) v) (finish))
        (when (= (mod v p) 0)
          (let ((count 0))
            (declare (type fixnum count))
            (iterate (while (= (mod v p) 0))
              (incf count)
              (setf v (truncate v p)))
            (push (cons p count) factors)))))
    (when (> v 1)
      (push (cons v 1) factors))
    factors))

(defun get-divisors (factors)
  (let ((divs (list 1)))
    (iterate (for f in factors)
      (let* ((p (car f))
             (c (cdr f))
             (new-divs nil))
        (iterate (for d in divs)
          (let ((mult d))
            (iterate (for i from 0 to c)
              (push mult new-divs)
              (setf mult (* mult p)))))
        (setf divs new-divs)))
    divs))

;; -----------------------------------------------------------------------------
;; メインルーチン
;; -----------------------------------------------------------------------------
(defun solve (&optional (limit 200000000000))
  (let* ((k-max (isqrt limit))
         (ans 0)
         (primes (make-primes k-max))
         (test-primes (make-test-primes primes))
         (odd-primes (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t)))
    
    ;; DFS用に奇数素数のみを抽出
    (iterate (for i from 1 below (length primes))
      (vector-push-extend (aref primes i) odd-primes))

    (format t "Starting Part 1 (m=2) target k-max = ~D...~%" k-max)
    ;; 不変量: k は偶数に限定される
    (iterate (for k from 2 to k-max by 2)
      (let* ((v (+ (- (* k k) k) 1))
             (factors (factorize v test-primes))
             (divs (get-divisors factors)))
        (iterate (for a in divs)
          (let ((b (truncate v a)))
            (when (< a b)
              (let ((p1 (+ k a))
                    (p2 (+ k b)))
                (when (<= (* p1 p2) limit)
                  (when (and (is-prime p1) (is-prime p2))
                    (incf ans (* p1 p2)))))))))
      
      (when (zerop (mod k 50000))
        (format t "Processed k = ~D / ~D, current ans = ~D~%" k k-max ans)))

    (format t "Starting Part 2 (m>=3) with highly pruned DFS...~%")
    (labels ((dfs (depth n phi p-idx)
               (let ((k-max-val (truncate n (- n phi))))
                 ;; 少なくとも2つの素数を選んだ状態で、最後の素数 p_last を一意に決定して検査
                 (when (>= depth 2)
                   (iterate (for k from 2 to k-max-val by 2)
                     (let ((num (+ (* k phi) 1))
                           (den (- (* k phi) (* (- k 1) n))))
                       (when (and (> den 0) (= (mod num den) 0))
                         (let ((p-last (truncate num den)))
                           (when (and (> p-last (aref odd-primes p-idx))
                                      (<= (* n p-last) limit)
                                      (is-prime p-last))
                             (incf ans (* n p-last)))))))))
               ;; 次の素数を追加（オイラー積 φ/n >= 1/2 の強烈な枝刈り）
               (iterate (for i from (1+ p-idx) below (length odd-primes))
                 (let* ((p (aref odd-primes i))
                        (next-n (* n p))
                        (next-phi (* phi (1- p))))
                   (if (> (* next-n p) limit) (finish))
                   (when (>= (* 2 next-phi) next-n)
                     (dfs (1+ depth) next-n next-phi i))))))
      
      (iterate (for i from 0 below (length odd-primes))
        (let* ((p (aref odd-primes i))
               (n p)
               (phi (1- p)))
          (if (> (* n p p) limit) (finish))
          (when (>= (* 2 phi) n)
            (dfs 1 n phi i)))))

    (format t "Final ans = ~D~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Part 1 (m=2) target k-max = 447213...
Processed k = 50000 / 447213, current ans = 62868625497020
Processed k = 100000 / 447213, current ans = 135567966251831
Processed k = 150000 / 447213, current ans = 205479120785626
Processed k = 200000 / 447213, current ans = 269635673307359
Processed k = 250000 / 447213, current ans = 286828184937440
Processed k = 300000 / 447213, current ans = 286828184937440
Processed k = 350000 / 447213, current ans = 286828184937440
Processed k = 400000 / 447213, current ans = 286828184937440
Starting Part 2 (m>=3) with highly pruned DFS...
Final ans = 288084712410001

User time    =       27.862
System time  =        0.175
Elapsed time =       28.407
Allocation   = 93226192 bytes
615 Page faults
GC time      =        0.009
 |------------------------------------------------------------|#
;;→ 288084712410001
:ok