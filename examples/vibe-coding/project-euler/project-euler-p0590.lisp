;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0590 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0590)
(declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; ==========================================================================
;;; Mathematical Projection & Invariants (数論的ショートカット)
;;; ==========================================================================
;;; 問題は H(L(N)) を求めること。
;;; L(N) は 1 から N までの最小公倍数。L(N) の各素因数 p の指数は a_p = floor(log_p N)。
;;; N=50000 の場合、a_p の最大値は a_2 = 15。つまり指数は 15 種類しかない。
;;;
;;; 包除原理より、H(L(N)) = \sum_{S} (-1)^{|S|} 2^{\prod_{p \notin S} (a_p+1) \prod_{p \in S} a_p} 
;;; 集合 S に入る素数の個数を、指数 a ごとに j_a 個とすると、同じ指数の素数の個数 C_a に対して
;;; 組み合わせは \binom{C_a}{j_a} 通り生じる。
;;;
;;; [フェルミ推定のレッドライン回避と次元崩壊]
;;; 巨大な肩の積 X をそのまま mod 10^9 で扱うと破綻する。
;;; 中国剰余定理 (CRT) により 10^9 = 512 * 1953125 へ分解する。
;;; X >= 9 であれば 2^X \equiv 0 (mod 512) は自明であるため、
;;; X' = X \pmod{1562500} を計算し、M_5 = 2^{X'} \pmod{1953125} から O(1) で 2^X \pmod{10^9} を復元する。
;;; 
;;; さらに、a=1 となる素数の個数 C_1 は 5085 と極端に大きい。
;;; C_1 以外の状態数は 38 * 6 * 3 * 2 * 2 * 2 * 2 = 10944 通りしかないため、
;;; Meet-in-the-Middle 的に a=2..15 を DFS で列挙し、最内ループで a=1 をインクリメンタルに計算する。
;;; これにより計算量は約 5.5 * 10^7 回へと崩壊し、Lisp上でコンマ数秒で完結する。
;;; ==========================================================================

(defparameter *total-sum* 0)
(declaim (type fixnum *total-sum*))

(defparameter *pow2-m5* (make-array 1562500 :element-type 'fixnum))
(defun init-pow2-m5 ()
  (let ((val 1))
    (declare (type fixnum val))
    (dotimes (i 1562500)
      (setf (aref *pow2-m5* i) val)
      (setf val (mod (* val 2) 1953125)))))

(defparameter *C* (make-array 16 :element-type 'fixnum :initial-element 0))
(defparameter *comb-tables* (make-array 16 :initial-element #()))

(declaim (inline combine capped-mul capped-expt mod-mul))

(defmacro mod-mul (a b)
  `(mod (* ,a ,b) #.(expt 10 9)))

;;; 中国剰余定理による復元: M_5 (mod 1953125) と M_2 = 0 (mod 512)
;;; 1953125 * 109 \equiv 1 (mod 512)
(defmacro combine (m5)
  (let ((k (gensym)))
    `(let ((,k (mod (* (- ,m5) 109) 512)))
       (mod (+ ,m5 (* ,k 1953125)) #.(expt 10 9)))))

(defun capped-mul (a b cap)
  (declare (type fixnum a b cap))
  (min (the fixnum (* a b)) cap))

(defun capped-expt (base power cap)
  (declare (type fixnum base power cap))
  (let ((res 1))
    (declare (type fixnum res))
    (dotimes (i power)
      (setf res (capped-mul res base cap)))
    res))

(defun mod-expt (base power modulus)
  (declare (type fixnum base power modulus))
  (let ((result 1)
        (b (mod base modulus))
        (p power))
    (declare (type fixnum result b p))
    (loop while (plusp p) do
      (when (oddp p)
        (setf result (mod (* result b) modulus)))
      (setf b (mod (* b b) modulus))
      (setf p (ash p -1)))
    result))

(defun get-primes (n)
  (let ((sieve (make-array (1+ n) :element-type 'bit :initial-element 0))
        (primes '()))
    (loop for i from 2 to n do
      (when (zerop (sbit sieve i))
        (push i primes)
        (loop for j from (* i i) to n by i do
          (setf (sbit sieve j) 1))))
    (nreverse primes)))

(defun get-a (p n)
  (let ((a 0) (curr p))
    (loop while (<= curr n) do
      (incf a)
      (setf curr (* curr p)))
    a))

(defun init-C (n)
  (fill *C* 0)
  (let ((primes (get-primes n)))
    (dolist (p primes)
      (let ((a (get-a p n)))
        (when (<= 1 a 15)
          (incf (aref *C* a)))))))

(defun build-nth-row (n modulus)
  (declare (type fixnum n modulus))
  (let ((row (make-array (1+ n) :element-type 'fixnum :initial-element 0)))
    (setf (aref row 0) 1)
    (loop for i from 1 to n do
      (loop for j from i downto 1 do
        (setf (aref row j) (mod (+ (aref row j) (aref row (1- j))) modulus))))
    row))

(defun init-comb-tables ()
  (loop for a from 1 to 15 do
    (let ((ca (aref *C* a)))
      (when (> ca 0)
        (setf (aref *comb-tables* a) (build-nth-row ca 1000000000))))))

;;; 最内ループ: a=1 (指数 1) の処理
(defun process-base (x-prime coef x-capped)
  (declare (type fixnum x-prime coef x-capped))
  (let ((c1 (aref *C* 1))
        (sum 0)
        (curr-x x-prime)
        (curr-x-capped x-capped)
        (comb-table (aref *comb-tables* 1)))
    (declare (type fixnum c1 sum curr-x curr-x-capped)
             (type (simple-array fixnum (*)) comb-table))
    (dotimes (k (1+ c1))
      (let* ((j1 (- c1 k))
             (actual-x curr-x-capped))
        (declare (type fixnum j1 actual-x))
        (let ((m (if (< actual-x 9)
                     (expt 2 actual-x)
                     (combine (aref *pow2-m5* curr-x)))))
          (declare (type fixnum m))
          (let* ((c1-term (aref comb-table j1))
                 (term-coef (mod-mul coef c1-term))
                 (term (mod-mul term-coef m)))
            (declare (type fixnum c1-term term-coef term))
            (if (oddp j1)
                (setf sum (mod (+ (- sum term) #.(expt 10 9)) #.(expt 10 9)))
                (setf sum (mod (+ sum term) #.(expt 10 9)))))))
      ;; 次の k に向けて O(1) で状態遷移
      (setf curr-x (mod (the fixnum (* curr-x 2)) 1562500))
      (setf curr-x-capped (capped-mul curr-x-capped 2 1000)))
    (setf *total-sum* (mod (the fixnum (+ *total-sum* sum)) #.(expt 10 9)))))

;;; 状態空間の再帰的列挙 (a=2..15)
(defun dfs (a current-x-prime current-coef current-x-capped)
  (declare (type fixnum a current-x-prime current-coef current-x-capped))
  (if (> a 15)
      (process-base current-x-prime current-coef current-x-capped)
      (let ((ca (aref *C* a)))
        (declare (type fixnum ca))
        (if (zerop ca)
            (dfs (1+ a) current-x-prime current-coef current-x-capped)
            (let ((comb-table (aref *comb-tables* a)))
              (declare (type (simple-array fixnum (*)) comb-table))
              (dotimes (j (1+ ca))
                (let* ((x-factor-capped (capped-mul (capped-expt a j 1000)
                                                    (capped-expt (1+ a) (- ca j) 1000)
                                                    1000))
                       (next-x-capped (capped-mul current-x-capped x-factor-capped 1000))
                       (x-factor (mod (* (mod-expt a j 1562500) 
                                         (mod-expt (1+ a) (- ca j) 1562500)) 
                                      1562500))
                       (next-x (mod (the fixnum (* current-x-prime x-factor)) 1562500))
                       (coef-factor (aref comb-table j))
                       (next-coef (mod-mul current-coef coef-factor)))
                  (declare (type fixnum x-factor-capped next-x-capped x-factor next-x coef-factor next-coef))
                  (when (oddp j)
                    (setf next-coef (mod (the fixnum (- #.(expt 10 9) next-coef)) #.(expt 10 9))))
                  (dfs (1+ a) next-x next-coef next-x-capped))))))))

(defun solve-for (n)
  (setf *total-sum* 0)
  (init-pow2-m5)
  (init-C n)
  (init-comb-tables)
  (dfs 2 1 1 1)
  *total-sum*)

(defun solve ()
  (format t "Verifying example HL(4) = ~A (Expected: 44)~%" (solve-for 4))
  (let ((ans (solve-for 50000)))
    (format t "Final Answer for HL(50000) = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0590:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Verifying example HL(4) = 44 (Expected: 44)
Final Answer for HL(50000) = 834171904

User time    =        4.350
System time  =        0.046
Elapsed time =        4.345
Allocation   = 383936 bytes
3700 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 834171904
:ok