;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0181 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0181)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)

#||
【数学的考察と次元崩壊の構築】
1. 問題の抽象化（2次元分割数）:
   $B$ 個の黒と $W$ 個の白のオブジェクトを任意のサイズのグループに分割する問題は、
   「非負整数のペア $(b, w)$ （ただし $(0,0)$ を除く）を要素とする完全ナップザック問題」、
   すなわち「2次元分割数（Bipartite Partitions）」に完全に帰着します。

2. フェルミ推定による計算量レッドラインの評価:
   可能なグループ（アイテム）の種類は $(B+1)(W+1) - 1$ 通りです。
   これらを順番に考え、DPで状態 $(b, w)$ を更新します。
   全体の演算回数は $\sum_{db=0}^B \sum_{dw=0}^W (B - db + 1)(W - dw + 1)$ となり、
   $B=60, W=40$ の場合、総加算回数は約 $1.5 \times 10^6$ 回に留まります。
   これはフェルミ推定のレッドライン（$10^7$ 回）を大幅に下回っています。
   したがって、この $\mathcal{O}(B^2 W^2)$ のDPアルゴリズムこそが「最適解」であり、2次元版のオイラーの五角数定理などを導入することは過剰な最適化（オーバーヘッドの増加）を招くだけの悪手であると結論づけられます。

3. アロケーションの最小化とBignumの安全性:
   サイズ $(B+1)(W+1) = 2501$ の1次元配列を1つだけ用意し、DPをインプレースで昇順走査します。
   Lispの `integer` 型（Bignum）を明示的に使用し、安全性を損なう `optimize` は掛けません。
   約150万回のBignum加算による短命オブジェクトが生成されますが、現代の世代別GCにとっては一瞬で回収される量であり、全く問題になりません。
||#

(defun solve-for (B-max W-max)
  (let* ((w-limit (1+ W-max))
         (size (* (1+ B-max) w-limit))
         ;; オーバーフローによる計算落ちを防ぐため、厳密な Bignum (integer) 配列を確保
         (dp (make-array size :element-type 'integer :initial-element 0)))
    
    ;; 初期状態: 0個のBと0個のWを分割する方法は1通り
    (setf (aref dp 0) 1)
    
    ;; 各グループサイズ (db, dw) をアイテムとしてDPを更新
    (iterate (for db from 0 to B-max)
      (iterate (for dw from 0 to W-max)
        (unless (and (= db 0) (= dw 0))
          ;; 1次元配列上の完全ナップザック（同じ要素を複数回使えるため昇順）
          (iterate (for b from db to B-max)
            (let ((b-offset (* b w-limit))
                  (prev-b-offset (* (- b db) w-limit)))
              (declare (type fixnum b-offset prev-b-offset))
              (iterate (for w from dw to W-max)
                (let ((idx (+ b-offset w))
                      (prev-idx (+ prev-b-offset (- w dw))))
                  (declare (type fixnum idx prev-idx))
                  (incf (aref dp idx) (aref dp prev-idx)))))))))
                  
    ;; 最後の要素が答え
    (aref dp (1- size))))

(defun solve ()
  (format t "観測: テストケース T(3,1) を検証中...~%")
  (let ((ans-test (solve-for 3 1)))
    (format t "観測: T(3,1) = ~D (Expected: 7)~%" ans-test))
  
  (format t "観測: 本探索 T(60,40) を実行中...~%")
  (let ((ans (solve-for 60 40)))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0181:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: テストケース T(3,1) を検証中...
観測: T(3,1) = 7 (Expected: 7)
観測: 本探索 T(60,40) を実行中...
Answer: 83735848679360680

User time    =        0.087
System time  =        0.011
Elapsed time =        0.060
Allocation   = 265616 bytes
3440 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 83735848679360680
:ok


#||
【CFFIアーキテクチャによる次元崩壊】
1. メモリアロケーションの完全なる外部化:
   DP配列の確保と解放 (`calloc` / `free`) をC言語のレイヤーに押し付けることで、
   Lisp側のガベージコレクタ (GC) はこの計算において文字通り「何もしない」状態になります。
   配列へのアクセスもネイティブのポインタ演算となるため、オーバーヘッドは皆無です。

2. CFFIによる型マッピング:
   C側の `uint64_t` は、CFFIの `:uint64` として定義することで、
   Lisp側へ戻る瞬間に安全に Lisp の `integer` 型としてボクシングされます。
   計算途中の150万回の加算はすべてレジスタ上のハードウェア演算で行われます。
||#

;; Cの共有ライブラリをロードする定義
(cffi:define-foreign-library libpe181
  (:darwin #.(make-pathname :name "libpe181" :type "dylib"
                          :defaults (translate-logical-pathname "pe:181")))
  (:windows (:or "libpe181.dll" "./libpe181.dll"))
  (t (:default "libpe181")))

;; ライブラリのロード
(cffi:use-foreign-library libpe181)

;; Cの関数 solve_181(int, int) -> uint64_t のバインディング
(cffi:defcfun ("solve_181" c-solve-181) :uint64
  (b-max :int)
  (w-max :int))

(defun solve-c ()
  (format t "観測: テストケース T(3,1) を C関数経由で検証中...~%")
  (let ((ans-test (c-solve-181 3 1)))
    (format t "観測: T(3,1) = ~D (Expected: 7)~%" ans-test))
  
  (format t "観測: 本探索 T(60,40) を C関数経由で実行中...~%")
  (let ((ans (c-solve-181 60 40)))
    (format t "Answer: ~D~%" ans)
    ans))


#+| Do it | (solve-c )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: テストケース T(3,1) を C関数経由で検証中...
観測: T(3,1) = 7 (Expected: 7)
観測: 本探索 T(60,40) を C関数経由で実行中...
Answer: 83735848679360680

User time    =        0.002
System time  =        0.000
Elapsed time =        0.001
Allocation   = 352 bytes
12 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 83735848679360680
:ok
