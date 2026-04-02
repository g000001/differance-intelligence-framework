;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0580 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0580)

#||
【数学的考察と次元崩壊の証明】
1. Hilbert数は H = 4k + 1 (k >= 0) の形をした整数。
2. 求める対象は、「1以外のいかなるHilbert数の平方でも割り切れないHilbert数（H-squarefree）」の個数。
   ヒルベルト数空間は一意分解整域ではないため（例：693 = 9*77 = 21*33）、
   素因数分解に基づいた単純なメビウス反転は破綻する。
3. しかし、条件を「通常の素因数分解」に写像することで劇的な次元崩壊が起きる。
   ある H-number n が H-squarefree であるための必要十分条件を考察すると、
   n = s^2 * k (k は無平方な奇数) としたとき、s (最大平方因子のbase) を割り切る H-number (h > 1) が存在してはならない。
   これが存在しないための条件は、
   「s が 1 mod 4 の素因数を持たず、かつ 3 mod 4 の素因数を高々1つしか持たない（つまり s = 1 または s = q ≡ 3 mod 4 の素数）」
   ことと完全に同値である。
4. したがって、カウントすべき n は、
   n = m^2 * k (k は 1 mod 4 の無平方数) として生成したとき、
   包除原理の係数 μ_new(m) が
   μ_new(m) = Σ_{d ∈ D, d|m} μ(m/d)
   (ここで D = {1} ∪ { q | q ≡ 3 mod 4, q は素数 })
   と表せるような加算に帰着する。
   これにより、計算量は O(2^N) の探索や複雑な動的計画法から完全に解放され、
   O(√N log log √N) という超高速な1次元配列上の篩（Sieve）に崩壊する。
||#

(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))

(defun solve (&optional (limit 10000000000000000))
  (declare (type (integer 1 4611686018427387903) limit))
  (let* ((sqrt-limit (isqrt limit))
         (half (ash sqrt-limit -1))
         ;; 奇数のみを扱うためインデックス i は 2i + 1 に対応する
         (is-prime (make-array half :element-type 'bit :initial-element 1))
         (mu (make-array half :element-type '(signed-byte 8) :initial-element 1))
         (primes3 (make-array 3000000 :element-type '(unsigned-byte 32) :adjustable t :fill-pointer 0)))
    (declare (type fixnum sqrt-limit half))
    
    (format t "観測: μ(m) 配列をエラトステネスの篩で構築中 (N=~D, √N=~D)...~%" limit sqrt-limit)
    (setf (sbit is-prime 0) 0) ; 1 は素数ではない
    
    ;; 通常のメビウス関数 μ(m) を計算
    (iterate (for i from 1 below half)
      (declare (type fixnum i))
      (when (= (sbit is-prime i) 1)
        (let ((p (1+ (ash i 1))))
          (declare (type fixnum p))
          ;; 3 mod 4 の素数を記録
          (when (= (logand p 3) 3)
            (vector-push-extend p primes3))
          ;; 素数の倍数の is-prime フラグを下ろす
          (iterate (for j from (+ i p) below half by p)
            (declare (type fixnum j))
            (setf (sbit is-prime j) 0))
          ;; 素数の倍数の μ を反転する
          (iterate (for j from i below half by p)
            (declare (type fixnum j))
            (setf (aref mu j) (- (aref mu j))))
          ;; 素数の平方の倍数の μ を 0 にする
          (let* ((p2 (* p p))
                 (start (ash (1- p2) -1)))
            (declare (type (integer 0 4611686018427387903) p2 start))
            (when (< start half)
              (iterate (for j from start below half by p2)
                (declare (type fixnum j))
                (setf (aref mu j) 0)))))))
                
    (format t "観測: μ_new(m) 配列をヒルベルト包除原理に基づいて構築中...~%")
    (let ((mu-new (make-array half :element-type '(signed-byte 8))))
      ;; D の要素 1 による寄与分
      (replace mu-new mu)
      ;; D の要素 q (≡ 3 mod 4 の素数) による寄与分を足し込む
      (iterate (for q in-vector primes3)
        (declare (type fixnum q))
        (let ((start (ash (1- q) -1)))
          (declare (type fixnum start))
          (iterate (for i from start below half by q)
                   (for j from 0)
                   (declare (type fixnum i j))
                   (incf (aref mu-new i) (aref mu j)))))
                   
      (format t "観測: テストケース T(10^7) を検証中...~%")
      (let ((ans-test 0))
        (declare (type fixnum ans-test))
        (iterate (for i from 0 below (ash (isqrt 10000000) -1))
          (declare (type fixnum i))
          (let ((mu-val (aref mu-new i)))
            (declare (type (signed-byte 8) mu-val))
            (when (not (= mu-val 0))
              (let* ((m (1+ (ash i 1)))
                     (m2 (* m m))
                     (n-div-m2 (truncate 10000000 m2))
                     (term (truncate (+ n-div-m2 3) 4)))
                (declare (type fixnum m m2 n-div-m2 term))
                (incf ans-test (* mu-val term))))))
        (format t "観測: T(10^7) = ~D (Expected: 2327192)~%" ans-test))
        
      (format t "観測: 巨大空間 T(10^16) への最終集計を実行中...~%")
      (let ((ans 0))
        (declare (type (integer 0 4611686018427387903) ans))
        (iterate (for i from 0 below half)
          (declare (type fixnum i))
          (let ((mu-val (aref mu-new i)))
            (declare (type (signed-byte 8) mu-val))
            (when (not (= mu-val 0))
              (let* ((m (1+ (ash i 1)))
                     (m2 (* m m))
                     (n-div-m2 (truncate limit m2))
                     (term (truncate (+ n-div-m2 3) 4)))
                (declare (type (integer 0 4611686018427387903) m m2 n-div-m2 term))
                (incf ans (* mu-val term))))))
        (format t "Answer: ~D~%" ans)
        ans))))

#+| Do it | (project-euler-0580:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: μ(m) 配列をエラトステネスの篩で構築中 (N=10000000000000000, √N=100000000)...
観測: μ_new(m) 配列をヒルベルト包除原理に基づいて構築中...
観測: テストケース T(10^7) を検証中...
観測: T(10^7) = 2327192 (Expected: 2327192)
観測: 巨大空間 T(10^16) への最終集計を実行中...
Answer: 2327213148095366

User time    =        3.458
System time  =        0.089
Elapsed time =        3.475
Allocation   = 118682984 bytes
44036 Page faults
GC time      =        0.036
 |------------------------------------------------------------|#
;;→ 2327213148095366
:ok
