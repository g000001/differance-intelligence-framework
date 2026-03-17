;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0272 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0272)

#||
(cl-text "Project Euler 272 Logic Projection - Refined"
  (cl-comment "1. Exact Integer Projection: C(n) = 242 means there are exactly 243 solutions to x^3 = 1 mod n. N(n) = 243.")
  (forall (n)
    (iff (= (C n) 242)
         (= (N n) 243)))

  (cl-comment "2. Multiplicative Function Reduction: By the Chinese Remainder Theorem, N(n) = product(N(p^k)). For p=1 mod 3, N(p^k)=3. For p=2 mod 3, N(p^k)=1. For p=3, N(3^1)=1, N(3^{k>=2})=3. Thus, to reach 243 = 3^5, n must have exactly 5 independent prime power components that contribute 3.")
  (= (N n) (^ 3 (count_contributing_components n)))

  (cl-comment "3. Dynamic Boundary Derivation: Removed the flawed manual calculation. The system now autonomously derives the absolute minimum threshold of A directly from the precomputed prime factors, guaranteeing memory safety.")
)
||#

(defun solve ()
  (let* ((MAX-N 100000000000)
         ;; 最大の A の素因数になり得る上限 (10^11 / (9 * 7 * 13 * 19) ≒ 6426322)
         (limit 6500000)
         (sieve (make-array (1+ limit) :element-type 'bit :initial-element 0))
         (primes1 (make-array 250000 :element-type 'fixnum :fill-pointer 0))
         ;; Project Eulerの巨大な解に備え、Bignumへの自動昇格を許容
         (total-sum 0))
    (declare (type integer total-sum)
             (type (unsigned-byte 64) MAX-N)
             (type fixnum limit))

    ;; 1. 高速線形篩による素数生成と、p ≡ 1 (mod 3) の抽出
    (setf (sbit sieve 0) 1 (sbit sieve 1) 1)
    (iterate
      (declare (type fixnum p))
      (for p from 2 to limit)
      (when (= (sbit sieve p) 0)
        (when (= (mod p 3) 1)
          (vector-push p primes1))
        (iterate
          (for i from (* p p) to limit by p)
          (setf (sbit sieve i) 1))))

    ;; 探索を枝刈り（Prune）するための「残り必要な素数の最小積」をキャッシュ
    (let ((min-products (make-array 6 :element-type '(unsigned-byte 64)))
          (num-primes1 (length primes1)))
      (setf (aref min-products 0) 1)
      (iterate
        (for i from 1 to 5)
        (setf (aref min-products i)
              (* (aref min-products (1- i))
                 (aref primes1 (1- i)))))

      ;; 人間の暗算によるハードコードを排除し、プログラム自身に真の境界を計算させる
      (let* ((min-a-dfs1 (aref min-products 5))         ;; DFS1の最小: 7 * 13 * 19 * 31 * 37 = 1978439
             (min-a-dfs2 (* 9 (aref min-products 4)))   ;; DFS2の最小: 9 * 7 * 13 * 19 * 31 = 482391
             (min-a (min min-a-dfs1 min-a-dfs2))
             (max-x (truncate MAX-N min-a))             ;; 真の最大空間 X = 207300
             (valid-m (make-array (1+ max-x) :element-type 'bit :initial-element 1))
             (sm (make-array (1+ max-x) :element-type '(unsigned-byte 64) :initial-element 0)))
        (declare (type fixnum max-x))

        ;; 2. 無害な積空間 M (p ≡ 2 mod 3 のみで構成) のプレフィックスサム O(X) を構築
        (iterate (for i from 3 to max-x by 3) (setf (sbit valid-m i) 0))
        (iterate
          (declare (type fixnum i))
          (for i from 0 below num-primes1)
          (for p = (aref primes1 i))
          (when (> p max-x) (leave))
          (iterate (for j from p to max-x by p)
            (setf (sbit valid-m j) 0)))

        (iterate
          (declare (type fixnum i))
          (for i from 1 to max-x)
          (setf (aref sm i)
                (+ (aref sm (1- i))
                   (if (= (sbit valid-m i) 1) i 0))))

        ;; 3. 再帰的深さ優先探索 (Zero-Allocation DFS)
        (labels ((dfs1 (p-idx needed a)
                   (declare (type fixnum p-idx needed)
                            (type (unsigned-byte 64) a))
                   (if (= needed 0)
                       ;; p ≡ 1 mod 3 を厳密に5つ選択したルート。3は未選択なので M と 3*M の双方が許容される。
                       (let* ((x (truncate MAX-N a))
                              (x3 (truncate x 3)))
                         (declare (type fixnum x x3))
                         (incf total-sum (* a (+ (aref sm x) (* 3 (aref sm x3))))))
                       (iterate
                         (declare (type fixnum i))
                         (for i from p-idx below num-primes1)
                         (for p = (aref primes1 i))
                         (for a-new = (* a p))
                         ;; フェルミ推定による絶対安全な枝刈り
                         (when (> (* a-new (aref min-products (1- needed))) MAX-N)
                           (leave))
                         (iterate
                           (while (<= a-new MAX-N))
                           (dfs1 (1+ i) (1- needed) a-new)
                           (setf a-new (* a-new p))))))

                 (dfs2 (p-idx needed a)
                   (declare (type fixnum p-idx needed)
                            (type (unsigned-byte 64) a))
                   (if (= needed 0)
                       ;; p ≡ 1 mod 3 を4つ、かつ 3^k (k>=2) を1つ選択したルート。M のみが許容される。
                       (let ((x (truncate MAX-N a)))
                         (declare (type fixnum x))
                         (incf total-sum (* a (aref sm x))))
                       (iterate
                         (declare (type fixnum i))
                         (for i from p-idx below num-primes1)
                         (for p = (aref primes1 i))
                         (for a-new = (* a p))
                         (when (> (* a-new (aref min-products (1- needed))) MAX-N)
                           (leave))
                         (iterate
                           (while (<= a-new MAX-N))
                           (dfs2 (1+ i) (1- needed) a-new)
                           (setf a-new (* a-new p)))))))

          (format t "Precomputation boundaries fixed. Max M size: ~A. Active Primes: ~A~%" max-x num-primes1)
          
          (format t "Executing DFS 1 (5 independent primes)...~%")
          (dfs1 0 5 1)

          (format t "Executing DFS 2 (4 independent primes + bounded powers of 3)...~%")
          (iterate
            (declare (type (unsigned-byte 64) p3))
            (for p3 first 9 then (* p3 3))
            (while (<= (* p3 (aref min-products 4)) MAX-N))
            (dfs2 0 4 p3))

          (format t "Space-time intersection complete.~%")
          total-sum)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputation boundaries fixed. Max M size: 207300. Active Primes: 222256
Executing DFS 1 (5 independent primes)...
Executing DFS 2 (4 independent primes + bounded powers of 3)...
Space-time intersection complete.

User time    =       13.543
System time  =        0.108
Elapsed time =       13.568
Allocation   = 719396224 bytes
4884 Page faults
GC time      =        0.009
 |------------------------------------------------------------|#
;;→ 8495585919506151122
:ok