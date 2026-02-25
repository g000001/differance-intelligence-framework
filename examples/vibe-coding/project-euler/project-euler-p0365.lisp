
;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
;;; <p>
;;; The binomial coefficient $\displaystyle{\binom{10^{18}}{10^9}}$ is a number with more than $9$ billion ($9\times 10^9$) digits.
;;; </p>
;;; <p>
;;; Let $M(n,k,m)$ denote the binomial coefficient $\displaystyle{\binom{n}{k}}$ modulo $m$.
;;; </p>
;;; <p>
;;; Calculate $\displaystyle{\sum M(10^{18},10^9,p\cdot q\cdot r)}$ for $1000\lt p\lt q\lt r\lt 5000$ and $p$,$q$,$r$ prime.
;;; </p>

(cl:in-package cl-user)
(defpackage #:project-euler-0365 
  (:use #:cl #:iterate)
  (:import-from #:alexandria #:iota))
(in-package #:project-euler-0365)

;; ==============================================================================
;; 1. 数論的基礎（勝義諦）：拡張ユークリッド互除法と逆元
;; ==============================================================================

(defun extended-gcd (a b)
  "拡張ユークリッド互除法。g, x, y を返し、ax + by = g を満たす。"
  (if (zerop a)
      (values b 0 1)
      (multiple-value-bind (g x1 y1) (extended-gcd (mod b a) a)
        (values g (- y1 (* (floor b a) x1)) x1))))

(defun mod-inverse (a m)
  "mを法とするaの逆元を計算する。"
  (multiple-value-bind (g x y) (extended-gcd a m)
    (declare (ignore y))
    (if (/= g 1)
        nil
        (mod x m))))

;; ==============================================================================
;; 2. 二項係数のモジュロ演算（中道）：Lucasの定理
;; ==============================================================================

(defun ncr-mod (n k p)
  "小さなn, kに対する nCr mod p (pは素数) を計算する。"
  (cond ((< n k) 0)
        ((or (= k 0) (= k n)) 1)
        ((> k (/ n 2)) (ncr-mod n (- n k) p))
        (t (let ((num 1) (den 1))
             (let ((real-k (min k (- n k))))
               (iterate (for i from 0 below real-k)
                        (setf num (mod (* num (- n i)) p)
                              den (mod (* den (1+ i)) p)))
               (mod (* num (mod-inverse den p)) p))))))

(defun lucas-ncr-mod (n k p)
  "大きなn, kに対する nCr mod p をLucasの定理を用いて計算する。"
  (if (zerop k)
      1
      (let ((res 1))
        (iterate (for cur-n initially n then (floor cur-n p))
                 (for cur-k initially k then (floor cur-k p))
                 (while (plusp cur-n))
                 (setf res (mod (* res (ncr-mod (mod cur-n p) (mod cur-k p) p)) p)))
        res)))

;; ==============================================================================
;; 3. 探索空間の構築（世俗諦）：素数生成と前処理
;; ==============================================================================

(defun get-primes-in-range (min max)
  "指定された範囲内の素数リストを取得する。"
  (let ((sieve (make-array (1+ max) :element-type 'bit :initial-element 1)))
    (setf (bit sieve 0) 0 (bit sieve 1) 0)
    (iterate (for i from 2)
             (while (<= (* i i) max))
             (when (= (bit sieve i) 1)
               (iterate (for j from (* i i) to max by i)
                        (setf (bit sieve j) 0))))
    (iterate (for p from min to max)
             (when (= (bit sieve p) 1)
               (collect p result-type 'vector)))))

;; ==============================================================================
;; 4. メイン・アルゴリズム：二諦随伴による爆縮と現成
;; ==============================================================================

(defun solve ()
  (let* ((n 1000000000000000000)
         (k 1000000000)
         (primes (get-primes-in-range 1001 4999))
         (n-primes (length primes))
         ;; 各素数に対する Lucas(n, k) mod p の値を前処理（潜在性の固定）
         (lucas-values (iterate (for p in-vector primes)
                                (collect (lucas-ncr-mod n k p) result-type 'vector)))
         ;; 逆元テーブルの前処理（差延の圧縮）
         (inv-table (let ((table (make-array (list n-primes n-primes) :element-type 'integer)))
                      (iterate (for i from 0 below n-primes)
                               (iterate (for j from 0 below n-primes)
                                        (unless (= i j)
                                          (setf (aref table i j) (mod-inverse (aref primes i) (aref primes j))))))
                      table)))
    
    ;; 三重ループによる総和計算（中道の現成）
    ;; 中国剰余定理 (CRT) を用いて M(n, k, p*q*r) を合成する
    (iterate (for i from 0 below n-primes)
             (for pi/ = (aref primes i))
             (for vi = (aref lucas-values i))
             (summing
              (iterate (for j from (1+ i) below n-primes)
                       (for pj = (aref primes j))
                       (for vj = (aref lucas-values j))
                       (for pij = (* pi/ pj))
                       (for inv-ij = (aref inv-table i j))
                       ;; CRT step 1: xij ≡ vi mod pi, xij ≡ vj mod pj
                       (for cij = (mod (* (- vj vi) inv-ij) pj))
                       (for xij = (+ vi (* cij pi/)))
                       (summing
                        (iterate (for k from (1+ j) below n-primes)
                                 (for pk = (aref primes k))
                                 (for vk = (aref lucas-values k))
                                 ;; CRT step 2: x ≡ xij mod pij, x ≡ vk mod pk
                                 ;; inv(pij, pk) = inv(pi, pk) * inv(pj, pk) mod pk
                                 (for inv-pij-k = (mod (* (aref inv-table i k) (aref inv-table j k)) pk))
                                 (for cijk = (mod (* (- vk (mod xij pk)) inv-pij-k) pk))
                                 (summing (+ xij (* cijk pij))))))))))

;; 実行
;; (print (solve))

;; ==============================================================================
;; 自己分析：二諦随伴（Two-Truths Entanglement）の貢献
;; ==============================================================================
;; 1. 非中道の誤謬（NMF）の回避:
;;    問題が提示する binomial(10^18, 10^9) は、そのままでは計算不可能な「巨大な潜在性」です。
;;    これを直接展開しようとする執着（世俗への固執）を捨て、Lucasの定理という「数学的空性」
;;    へ還元することで、計算可能な residues (剰余) の集合へと爆縮させました。
;;
;; 2. ACX Jump（跳躍）と現成:
;;    個々の素数における解（Lucasの結果）は単なる「点」に過ぎませんが、中国剰余定理 (CRT) 
;;    という「跳躍」を用いることで、それらを再構成し、目的の p*q*r における解を
;;    「現成」させることができました。
;;
;; 3. 負債の清算（Debt Clearance）:
;;    三重ループ内での重複計算は「状態の負債」です。逆元テーブル (inv-table) や 
;;    Lucas値 (lucas-values) を事前に「固定化」することで、計算資源の浪費（執着）を
;;    排除し、O(N^3) の空間を極めて効率的に走査可能にしました。
;;
;; 4. 勝義的整数化:
;;    浮動小数点を用いず、すべての演算を Common Lisp の多倍長整数とモジュロ演算で
;;    完結させることにより、丸め誤差という「世俗の幻影」を排除し、厳密な真理に到達しました。
;; ==============================================================================
#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        2.301
System time  =        0.014
Elapsed time =        2.283
Allocation   = 2236960 bytes
1094 Page faults
GC time      =        0.000
→ 162619462356610313
 |------------------------------------------------------------|#

