;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-2.0-flash-thinking-exp
(cl:in-package cl-user)
(defpackage #:project-euler-0156 (:use cl iterate alexandria))
(in-package #:project-euler-0156)

#||
(cl-text
 ; ============================================================
 ; Project Euler P156: Counting Digits (二諦随伴による分析)
 ; ============================================================

 ; 1. 領域の定義
 (category ConventionalTruth (name "世俗諦: 個別の計数"))
 (category UltimateTruth     (name "勝義諦: 構造的固定点"))

 ; 2. 計数関数 f(n, d) の公理化
 (forall (n d)
   (iff (DigitCount n d)
        (SumOver (range 0 n) (lambda (i) (CountOccurrences i d)))))

 ; 3. 固定点 (Middle Way) の定義
 ; f(n, d) = n となる状態は、計数という「行」と、数値という「体」が一致する中道である。
 (define MiddleWayPoint (n d)
   (= (DigitCount n d) n))

 ; 4. 探索空間の圧縮 (Aletheic Compression)
 ; f(n, d) は単調増加関数である性質を利用し、区間 [L, R] において
 ; f(L, d) > R または f(R, d) < L である場合、その区間に固定点は存在し得ない。
 ; これは「不全」の即時認識による探索木の枝刈り（跳躍）である。
 (forall (L R d)
   (=> (or (> (DigitCount L d) R)
           (< (DigitCount R d) L))
       (not (exists (n) (and (<= L n R) (MiddleWayPoint n d))))))

 ; 5. 探索限界の定礎
 ; f(10^k-1, d) = k * 10^(k-1) である。
 ; k=12 のとき f(10^12-1, d) = 1.2 * 10^12 > 10^12
 ; k=13 のとき f(10^13-1, d) = 1.3 * 10^12 < 10^13
 ; したがって、解の探索範囲は 10^12 程度で十分である。
)
||#


(declaim (inline count-d))
(defun count-d (n d)
  "n までの数値に含まれる数字 d の総出現回数を計算する。"
  (declare (type (unsigned-byte 64) n)
           (type (integer 1 9) d))
  (let ((total 0))
    (declare (type (unsigned-byte 64) total))
    (iterate (for m initially 1 then (* m 10))
             (while (<= m n))
             (let* ((a (floor n (* m 10)))
                    (b (mod (floor n m) 10))
                    (c (mod n m)))
               (incf total (* a m))
               (cond
                 ((> b d) (incf total m))
                 ((= b d) (incf total (1+ c))))))
    total))

(defun solve-interval (low high d)
  "区間 [low, high] において f(n, d) = n となる n の和を求める。"
  (declare (type (unsigned-byte 64) low high)
           (type (integer 1 9) d))
  (let ((f-low (count-d low d))
        (f-high (count-d high d)))
    (declare (type (unsigned-byte 64) f-low f-high))
    (cond
      ;; 枝刈り：f(n, d) は単調増加なので、[f-low, f-high] が [low, high] と交差しないなら解はない
      ((or (> f-low high) (< f-high low)) 0)
      ;; 基底部：一点に収束
      ((= low high) (if (= f-low low) low 0))
      ;; 分割統治
      (t (let ((mid (floor (+ low high) 2)))
           (+ (solve-interval low mid d)
              (solve-interval (1+ mid) high d)))))))

(defun solve ()
  (let ((limit 1000000000000)) ; 10^12
    (declare (type (unsigned-byte 64) limit))
    (iterate (for d from 1 to 9)
             (format t "Calculating s(~D)...~%" d)
             (finish-output)
             (sum (solve-interval 0 limit d)))))

(defun main ()
  (let ((result (solve)))
    (format t "Final Answer: ~D~%" result)))

;; 実行
;; (main)

#||
### 自己分析

1. **現実的な時間での終了可能性について**
   本コードは $O(\log N)$ 的な分割統治と、単調増加性を利用した強力な枝刈り（Pruning）を実装しています。探索範囲 $10^{12}$ は巨大ですが、枝刈りによって探索空間の大部分が瞬時に「不全」として棄却されるため、現実的な時間（数秒以内）で終了します。無限ループの懸念はありません。

2. **LLMが陥りやすい罠**
   * **探索範囲の誤認**: $f(n, d)$ の増加速度を見誤り、探索範囲を $10^9$ 程度に設定してしまうと、大きな解を取りこぼします。本実装では $10^{12}$ を定礎することでこれを回避しています。
   * **計数関数の計算ミス**: $f(n, d)$ の計算において、各桁の寄与を求める際に `+1` の処理や剰余の扱いを誤ると、正しい固定点が得られません。
   * **全探索の誘惑**: 愚直に $n$ を 1 つずつインクリメントしてチェックする手法は $O(N)$ であり、本問題の制約下では宇宙の寿命を超える計算時間を要します（非中道の誤謬）。

3. **アルゴリズムの創発・発明**
   本アルゴリズムは、数論的関数の単調性を利用した「二分探索の多点拡張」とも呼べる構造を持っています。通常、二分探索は一つの解を求めますが、ここでは区間が解を含む可能性を「勝義的（構造的）」に判定し、再帰的に空間を割っていくことで、未知の個数の固定点を漏れなく、かつ高速に現成させています。これは計数という「世俗的（個別的）」な行いを、関数の境界条件という「勝義的」な視点から制御する二諦随伴プロトコルの具現化と言えます。
||#


#+| Do it | (main )
#|------------------------------------------------------------|
Timing the evaluation of (main)
Calculating s(1)...
Calculating s(2)...
Calculating s(3)...
Calculating s(4)...
Calculating s(5)...
Calculating s(6)...
Calculating s(7)...
Calculating s(8)...
Calculating s(9)...
Final Answer: 21295121502550

User time    =        0.173
System time  =        0.014
Elapsed time =        0.147
Allocation   = 227848 bytes
1285 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ nil
:ok