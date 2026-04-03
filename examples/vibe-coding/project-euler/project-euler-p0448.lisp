;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0448 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0448)

#||
【数学的考察と次元崩壊の証明】
1. A(n)の式変形:
   $A(n) = \frac{1}{n} \sum_{i=1}^n \operatorname{lcm}(n, i) = \sum_{i=1}^n \frac{i}{\gcd(n, i)}$
   $d = \gcd(n, i)$ として和の順序を整理すると、互いに素な整数の和の公式により、
   $A(n) = \frac{1}{2} + \frac{1}{2} \sum_{d|n} d \cdot \varphi(d)$
   となる。

2. S(N)への展開:
   $S(N) = \sum_{n=1}^N A(n) = \frac{N}{2} + \frac{1}{2} \sum_{n=1}^N \sum_{d|n} d \cdot \varphi(d)$
   和の順序を交換すると、
   $S(N) = \frac{N}{2} + \frac{1}{2} \sum_{d=1}^N d \cdot \varphi(d) \lfloor \frac{N}{d} \rfloor$

3. 杜教篩 (Dirichlet Prefix Sum) による O(N^(2/3)) への崩壊:
   $F(x) = \sum_{d=1}^x d \cdot \varphi(d)$ を高速に求める必要がある。
   Dirichlet convolution において、$g(n) = n\varphi(n)$ と $h(n) = n$ を畳み込むと、
   $(g * h)(n) = \sum_{d|n} d\varphi(d) \frac{n}{d} = n \sum_{d|n} \varphi(d) = n^2$ となる。
   この両辺のプレフィックスサムをとることで、次の漸化式が得られる。
   $\sum_{n=1}^x n^2 = \sum_{d=1}^x d \cdot F(\lfloor \frac{x}{d} \rfloor)$
   変形して、
   $F(x) = \frac{x(x+1)(2x+1)}{6} - \sum_{d=2}^x d \cdot F(\lfloor \frac{x}{d} \rfloor)$

4. 演算の最適化:
   N = 10^11 クラスの探索空間において、K = 5,000,000 までをエラトステネスの篩による配列で事前計算し、
   残りの x については $\lfloor x/d \rfloor$ が連続する区間をまとめて計算（平方分割トリック）し、ハッシュにメモ化する。
   これにより計算量は $\mathcal{O}(N^{2/3})$ に抑えられ、約 $4 \times 10^7$ 回のループで完結する。
||#

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +MOD+ 999999017)
(defconstant +K+ 5000000)

(defvar *F-array*)
(defvar *F-hash*)

(defun precompute ()
  "Kまでの F(x) を線形時間で事前構築する"
  (setf *F-array* (make-array (1+ +K+) :element-type '(unsigned-byte 32)))
  (let ((phi (make-array (1+ +K+) :element-type '(unsigned-byte 32))))
    (iterate (for i from 1 to +K+)
      (setf (aref phi i) i))
    ;; エラトステネスの篩による Totient 関数の計算
    (iterate (for i from 2 to +K+)
      (when (= (aref phi i) i)
        (iterate (for j from i to +K+ by i)
          (setf (aref phi j) (truncate (* (aref phi j) (1- i)) i)))))
    ;; F(x) の累積和
    (setf (aref *F-array* 0) 0)
    (iterate (for i from 1 to +K+)
      (let ((term (mod (* i (aref phi i)) +MOD+)))
        (setf (aref *F-array* i) (mod (+ (aref *F-array* (1- i)) term) +MOD+))))))

(declaim (inline S1 S2))

(defun S1 (x)
  "Sum_{i=1}^x i  (mod MOD)"
  (declare (type (integer 0 4611686018427387903) x))
  (let* ((xm (mod x +MOD+))
         (x1m (mod (1+ x) +MOD+)))
    ;; 499999509 は MOD における 2 の逆元
    (mod (* xm x1m 499999509) +MOD+)))

(defun S2 (x)
  "Sum_{i=1}^x i^2  (mod MOD)"
  (declare (type (integer 0 4611686018427387903) x))
  (let* ((xm (mod x +MOD+))
         (x1m (mod (1+ x) +MOD+))
         (x2m (mod (1+ (* 2 x)) +MOD+)))
    ;; 166666503 は MOD における 6 の逆元
    (mod (* xm (mod (* x1m x2m) +MOD+) 166666503) +MOD+)))

(defun get-F (x)
  "メモ化再帰とブロック分割による F(x) の高速計算"
  (declare (type (integer 0 4611686018427387903) x))
  (if (<= x +K+)
      (aref *F-array* x)
      (multiple-value-bind (val present-p) (gethash x *F-hash*)
        (if present-p
            val
            (let ((ans (S2 x))
                  (L 2))
              ;; floor(x/d) が同一の値をとる区間 [L, R] でまとめて計算する
              (iterate (while (<= L x))
                (let* ((v (truncate x L))
                       (R (truncate x v))
                       (s1-diff (mod (- (S1 R) (S1 (1- L))) +MOD+)))
                  (setf ans (mod (- ans (* s1-diff (get-F v))) +MOD+))
                  (setf L (1+ R))))
              (setf (gethash x *F-hash*) ans))))))

(defun solve-for (N)
  "最終的な S(N) を導出する"
  (declare (type (integer 0 4611686018427387903) N))
  (let ((W 0)
        (L 1))
    ;; 同様にブロック分割を用いて Sum( d*phi(d) * floor(N/d) ) を計算
    (iterate (while (<= L N))
      (let* ((v (truncate N L))
             (R (truncate N v))
             (f-diff (mod (- (get-F R) (get-F (1- L))) +MOD+)))
        (setf W (mod (+ W (* (mod v +MOD+) f-diff)) +MOD+))
        (setf L (1+ R))))
    (let* ((N-mod (mod N +MOD+))
           ;; S(N) = (N + W) * (1/2) (mod MOD)
           (ans (mod (* (+ N-mod W) 499999509) +MOD+)))
      ans)))

(defun solve ()
  (format t "観測: 事前計算 (K=~D) を開始...~%" +K+)
  (precompute)
  (setf *F-hash* (make-hash-table :test 'eql))
  
  (format t "観測: テストケース S(100) を検証中...~%")
  (let ((ans100 (solve-for 100)))
    (format t "観測: S(100) mod ~D = ~D (Expected: 122726)~%" +MOD+ ans100))
    
  (format t "観測: 本探索 S(99999999019) を実行中...~%")
  (let ((ans (solve-for 99999999019)))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0448:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: 事前計算 (K=5000000) を開始...
観測: テストケース S(100) を検証中...
観測: S(100) mod 999999017 = 122726 (Expected: 122726)
観測: 本探索 S(99999999019) を実行中...
Answer: 106467648

User time    =       24.833
System time  =        0.178
Elapsed time =       24.959
Allocation   = 739066008 bytes
22193 Page faults
GC time      =        0.030
 |------------------------------------------------------------|#
;;→ 106467648
:ok