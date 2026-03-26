;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0953 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0953)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

#||
【自己批判と数論的ショートカットの証明】
巨大なN=10^14のNim和シミュレーションは、無平方数(m)への分解により次元崩壊する。
さらに、XOR和が0になるためには最大の素因数 pk が pk < 2 * p_{k-1} を満たす必要があり、
これにより「mの最大素因数は必ず 10^7 以下になる」という強烈な不変量が導かれる。
この制約を利用し、10^7までの素数表上で「最後の素数は計算で一意に決定する」という
O(N^(1/2)) のスパースなDFSを行うことで、探索空間は数千万ノードに圧縮され、
力技のエージェント探索を不要にする美しいLispのアルゴリズムへと昇華される。
||#

(defconstant $modulo 1000000007)

(defun calc-s2 (limit-k)
  "sum_{s=1}^{K} s^2 mod $modulo をO(1)で計算する"
  (mod (truncate (* limit-k (* (+ limit-k 1) (+ (* 2 limit-k) 1))) 6) $modulo))

(defun build-primes (limit-val)
  "エラトステネスの篩による素数表の構築"
  (let ((is-prime-array (make-array (1+ limit-val) :element-type 'bit :initial-element 1))
        (primes-array (make-array (truncate limit-val 6) :element-type 'fixnum :fill-pointer 0)))
    (setf (sbit is-prime-array 0) 0
          (sbit is-prime-array 1) 0)
    (iterate ((prime-p (scan-range :from 2 :upto limit-val)))
      (when (= (sbit is-prime-array prime-p) 1)
        (vector-push prime-p primes-array)
        (when (<= (* prime-p prime-p) limit-val)
          (iterate ((index-j (scan-range :from (* prime-p prime-p) :upto limit-val :by prime-p)))
            (setf (sbit is-prime-array index-j) 0)))))
    (values is-prime-array primes-array)))

(defun solve (&optional (limit-n #.(expt 10 14)))
  ;; m <= 10^14 のとき、素因数の制約により pk <= sqrt(10^14 / 2) < 7.08 * 10^6 となる。
  ;; 余裕を持たせて 10^7 まで生成すれば物理的な配列外参照は起こり得ない。
  (let* ((limit-p (min 10000000 (+ (isqrt (truncate limit-n 2)) 100))))
    (format t "観測: 素数表を ~D まで生成します~%" limit-p)
    (multiple-value-bind (is-prime-array primes-array) (build-primes limit-p)
      (let ((num-primes (length primes-array))
            (total-sum 0))
        
        ;; 1. m = 1 の場合 (空のNim山 = XOR和 0)
        (let ((k-val (isqrt limit-n)))
          (setf total-sum (calc-s2 k-val)))
        
        ;; 2. m >= 2 の場合 (DFSによる無平方数の探索)
        (labels ((dfs-search (last-idx current-m current-xor)
                   (do ((index-i (1+ last-idx) (1+ index-i)))
                       ((>= index-i (- num-primes 2)))
                     (let* ((prime-p (aref primes-array index-i))
                            (next-prime (aref primes-array (1+ index-i))))
                       
                       ;; 枝刈り: 今選んだ素数と次の素数を掛けて上限を超えるなら、これ以上深くも潜れず上がりも作れない
                       (when (> (* current-m prime-p next-prime) limit-n)
                         (return))
                       
                       (let ((next-m (* current-m prime-p))
                             (next-xor (logxor current-xor prime-p)))
                         
                         ;; 上がり判定: XOR和が最後の素数 pk になり得るかチェック
                         (when (and (> next-xor prime-p)
                                    (< next-xor limit-p)
                                    (= (sbit is-prime-array next-xor) 1)
                                    (<= (* next-m next-xor) limit-n))
                           (let* ((m-val (* next-m next-xor))
                                  (k-val (isqrt (truncate limit-n m-val)))
                                  (term (mod (* (mod m-val $modulo) (calc-s2 k-val)) $modulo)))
                             (setf total-sum (mod (+ total-sum term) $modulo))))
                         
                         ;; DFS深掘り判定: さらに次の素数を選んで潜る余地があるか
                         (when (<= (* next-m next-prime (aref primes-array (+ index-i 2))) limit-n)
                           (dfs-search index-i next-m next-xor)))))))
          
          (format t "観測: DFS探索を開始します~%")
          ;; 探索の起点は p1 (最初の素数)。k>=3 なので p1, p2, p3 の積が limit-n 以下でなければならない
          (do ((index-i 0 (1+ index-i)))
              ((>= index-i (- num-primes 2)))
            (let* ((prime1 (aref primes-array index-i))
                   (prime2 (aref primes-array (1+ index-i)))
                   (prime3 (aref primes-array (+ index-i 2))))
              (when (> (* prime1 prime2 prime3) limit-n)
                (return))
              (dfs-search index-i prime1 prime1)))
              
          (format t "S(~D) = ~D~%" limit-n total-sum)
          total-sum)))))

#+| Do it | (project-euler-0953:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: 素数表を 7071167 まで生成します
観測: DFS探索を開始します
S(100000000000000) = 176907658

User time    =  0:01:39.844
System time  =        0.369
Elapsed time =  0:01:40.321
Allocation   = 3474771200 bytes
12714 Page faults
GC time      =        0.072
 |------------------------------------------------------------|#
;;→ 176907658
:ok