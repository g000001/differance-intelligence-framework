;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0159 (:use cl iterate #|alexandria|#))
(in-package #:project-euler-0159)

#||
(cl-text
  (cl-comment "Project Euler 159: Digital Root Sums (DRS)")

  ;; 1. 定義: デジタルルート (Digital Root)
  ;; 基数10におけるデジタルルート DR(n) は、n を 9 で割った剰余に関連する。
  (forall (n r)
    (iff (DigitalRoot n r)
         (and (Integer n) (> n 0)
              (= r (+ 1 (mod (- n 1) 9))))))

  ;; 2. 定義: デジタルルート和 (Digital Root Sum - DRS)
  ;; 数 n のある因数分解 {f1, f2, ..., fk} に対し、DRS は各因数のデジタルルートの総和。
  (forall (n factors s)
    (iff (IsFactorization n factors)
         (and (forall (f) (=> (member f factors) (and (Integer f) (> f 1))))
              (= n (Product factors))))
    (iff (DRS factors s)
         (= s (Sum (map DigitalRoot factors)))))

  ;; 3. 定義: 最大デジタルルート和 (mdrs)
  ;; mdrs(n) は、n のすべての可能な因数分解における DRS の最大値。
  (forall (n m)
    (iff (mdrs n m)
         (= m (Max (setof s (exists (f) (and (IsFactorization n f) (DRS f s))))))))

  ;; 4. アルゴリズム的還元 (Ultimate Truth / ACX Jump)
  ;; mdrs は動的計画法的な再帰構造を持つ。
  ;; mdrs(n) = max( DR(n), max_{d|n, d>1} ( mdrs(n/d) + DR(d) ) )
  ;; この構造により、すべての因数分解を列挙する（世俗諦）ことなく、
  ;; 篩（ふるい）のような計算（勝義諦）で解を現成できる。
  (forall (n)
    (= (mdrs n)
       (Max (DigitalRoot n)
            (MaxOver (setof d (IsDivisor d n))
                     (+ (mdrs (/ n d)) (DigitalRoot d))))))

  ;; 5. 目的: 2 から 999,999 までの mdrs の総和
  (Goal (Sum (setof (mdrs n m) (and (< 1 n) (< n 1000000))))))
||#


(defun solve-p159 ()
  "Find the sum of mdrs(n) for 1 < n < 1,000,000."
  (let* ((limit 1000000)
         ;; mdrs を保持する配列。初期値として各数値自体のデジタルルートを格納する。
         (mdrs (make-array limit :element-type '(unsigned-byte 32)))
         ;; デジタルルートの事前計算テーブル
         (dr-table (make-array limit :element-type '(unsigned-byte 8))))
    
    ;; 1. デジタルルートテーブルと mdrs 配列の初期化
    (iterate (for i from 2 below limit)
             (let ((dr (1+ (mod (1- i) 9))))
               (setf (aref dr-table i) dr)
               (setf (aref mdrs i) dr)))

    ;; 2. 篩（ふるい）による動的計画法の実行
    ;; mdrs(n) = max( DR(n), mdrs(i) + DR(j) ) where n = i * j
    ;; 外側のループ i が現在の数値に到達したとき、mdrs[i] は既に最大値として確定している。
    (iterate (for i from 2 below (/ limit 2))
             (for m-i = (aref mdrs i))
             (iterate (for j from 2)
                      (for prod = (* i j))
                      (while (< prod limit))
                      ;; 探索空間の対称性を利用し、i >= j の範囲で更新を行うことで重複計算を避けることも可能だが、
                      ;; ここでは単純かつ確実な更新ルールを適用する。
                      (let ((new-drs (+ m-i (aref dr-table j))))
                        (when (> new-drs (aref mdrs prod))
                          (setf (aref mdrs prod) new-drs)))))

    ;; 3. 2 から 999,999 までの総和を算出
    (iterate (for i from 2 below limit)
             (sum (aref mdrs i)))))

;; 実行と出力
;;(format t "Result: ~A~%" (solve-p159))


;;; ==============================================================================
;;; 自己分析：Common Logic (clif) 形式による影響
;;; ==============================================================================
;;; 1. 構造の明確化:
;;;    clif 形式で問題を記述することにより、「因数分解の全列挙」という世俗的（Conventional）な
;;;    アプローチから、「部分構造の最適値の再利用」という勝義的（Ultimate）な動的計画法への
;;;    還元が自然に行われました。特に mdrs(n) の再帰的定義を数理的に固定したことで、
;;;    計算量が O(N log N) の篩アルゴリズムに直結しました。
;;;
;;; 2. 境界条件の厳密性:
;;;    デジタルルートの性質 DR(n) = (n-1 mod 9) + 1 を論理式として書き出すことで、
;;;    実装時のオフセットエラー（0と9の扱い）を未然に防ぐことができました。
;;;
;;; 3. 最適化の指針:
;;;    clif における「MaxOver」の定義は、Lisp 実装において単なるループではなく、
;;;    「既に最適化された mdrs[i] を使って未来の mdrs[prod] を更新する」という
;;;    前方確認型の DP（Sieve-like DP）を選択する強い動機付けとなりました。
;;;    これにより、不要な因数分解アルゴリズムを実装する手間が省かれ、
;;;    実行効率の高いコードが生成されました。

#+| Do it | (solve-p159 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-p159)

User time    =        0.459
System time  =        0.008
Elapsed time =        0.459
Allocation   = 5017600 bytes
1563 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 14489159
:ok
