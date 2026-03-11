;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0957 (:use cl iterate alexandria))
(in-package #:project-euler-0957)

#||
(cl-text euler-acx
  (cl-comment "Ontology for Euler-ACX: Aletheic Context for Project Euler P957")
  
  (cl-comment "=== 1. ACX Jump: Geometric Projection to Minkowski Sums ===")
  (cl-comment "The geometric construction of lines passing through fixed red points (triangle) 
  and blue points translates algebraically to coordinates given by Minkowski sums of line segments.")
  (cl-comment "Let A_n, B_n, C_n be the sets of parameter coordinates for the blue points at step n.
  These sets form expanding hexagonal regions in a 2D lattice. The recurrence is A_n = -B_{n-1} - C_{n-1}.")
  
  (cl-comment "=== 2. Inclusion-Exclusion on the Intersection Space ===")
  (cl-comment "The total number of blue points g(n) is determined by the intersections of lines 
  originating from the red points. By the inclusion-exclusion principle on the Cartesian products 
  A_{n-1}*B_{n-1}, etc., we get: g(n) = 3 |A_{n-1}|^2 - 2 |I_{ABC}(n-1)|, 
  where I_{ABC} is the common intersection corresponding to integer points in a 4D polytope.")
  
  (cl-comment "=== 3. Middle Way: Ehrhart Polynomial Interpolation ===")
  (cl-comment "For n=16 (g(16)), the side lengths of the hexagons reach m = (2^{15}-2)/3 = 10922.
  Directly counting lattice points in the 4D polytope would take O(m^4) ~ 1.4*10^16 operations.
  However, by Ehrhart's theory of lattice polytopes, the number of lattice points is exactly 
  a 4th-degree polynomial in m. We calculate the first 6 values (m=0 to 5) in milliseconds, 
  then interpolate to m=10922 in O(1) time. This is the ultimate Debt Clearance.")
)
||#

;; 4Dポリトープの格子点数を素朴なループで数える関数（mが小さい時のみ使用）
(defun count-I-ABC (m)
  (let ((count 0)
        (min-m-1 (- -1 m))
        (m+1 (1+ m))
        (min-m (- m)))
    (iterate (for xa from min-m-1 to m)
      (iterate (for ya from min-m-1 to m+1)
        (let ((xa-ya (- xa ya)))
          (when (and (<= min-m-1 xa-ya) (<= xa-ya m))
            (iterate (for xb from min-m-1 to m+1)
              (let ((xa+xb (+ xa xb)))
                (when (and (<= min-m-1 xa+xb) (<= xa+xb m))
                  (iterate (for yb from min-m-1 to m)
                    (let ((xb-yb (- xb yb))
                          (ya+yb (+ ya yb)))
                      (when (and (<= min-m xb-yb) (<= xb-yb m+1)
                                 (<= min-m-1 ya+yb) (<= ya+yb m))
                        (let ((x-y-sum (+ xa-ya xb-yb)))
                          (when (and (<= min-m-1 x-y-sum) (<= x-y-sum m+1))
                            (incf count)))))))))))))
    count))

;; m=0~4までのデータから4次多項式の差分テーブルを構築し、念のためm=5でアサートする
(defun check-polynomial ()
  (let ((vals (make-array 5)))
    (iterate (for i from 0 to 4)
      (setf (aref vals i) (count-I-ABC i)))
    (let ((d (copy-seq vals)))
      (iterate (for i from 1 to 4)
        (iterate (for j from 4 downto i)
          (setf (aref d j) (- (aref d j) (aref d (1- j))))))
      ;; m=5 を予測して検証
      (let ((ans (aref d 0))
            (term 1)
            (x 5))
        (iterate (for i from 1 to 4)
          (setf term (truncate (* term (- x (1- i))) i))
          (incf ans (* term (aref d i))))
        (let ((actual (count-I-ABC 5)))
          (unless (= ans actual)
            (error "Not a pure 4th degree polynomial! Expected ~A, got ~A" actual ans))
          d)))))

(defun solve ()
  (let* ((d (check-polynomial))
         (m 10922) ; n=16のとき、(2^15 - 2)/3 = 10922
         (ans-I (aref d 0))
         (term 1))
    
    ;; m=10922 に対する I_{ABC}(15) の値を多項式補間でO(1)計算
    (iterate (for i from 1 to 4)
      (setf term (truncate (* term (- m (1- i))) i))
      (incf ans-I (* term (aref d i))))
    
    ;; N_{15} = |A_{15}| の計算 (公式: 3m^2 + 7m + 4)
    (let* ((N15 (+ (* 3 m m) (* 7 m) 4))
           ;; g(n) = 3 * N_{n-1}^2 - 2 * |I_{ABC}(n-1)|
           (ans (- (* 3 N15 N15) (* 2 ans-I))))
      (format t "g(16) = ~A~%" ans)
      ans)))

;;; (solve)

#||
## 自己分析 (Self-Analysis)

* **実行時間の現実性 (Termination & Real-Time Viability):**
  本アルゴリズムは遅いインタプリタ環境であっても **0.01秒未満で即座に完了** します。重い計算に見える `count-I-ABC` は高々 $m=5$（ループ回数約2万回、数ミリ秒）までしか呼び出されません。巨大な $g(16)$ の実態である $m=10922$ の計算は、補間公式の数回の四則演算（$O(1)$）へと次元削減されているため、無限ループやタイムアウトの可能性は一切ありません。

* **LLMが陥りやすい罠 (LLM Traps / Illusions):**
  
  本問題の最大の罠は「幾何学的シミュレーションの直接実装（世俗への執着）」です。2日目で28個に増える点が、16日目には約 $10^{17}$ 個に爆発します。これをリストやハッシュセットで管理しようとする解法は、実行前にメモリが枯渇し破綻します。
  また、前回の教訓を活かし、不要な最適化宣言 `(optimize (speed 3) (safety 0))` を一切排除しました。これにより、計算結果が安全かつ自動的に Lisp の多倍長整数（Bignum）に委譲され、サイレントなオーバーフロー（悪取空）の罠を完全に回避しています。

* **アルゴリズムの創発 (Emergence & Inventions):**
  最も美しい創発は、フラクタル的に増殖する幾何学的な直線の交点を、ミンコフスキー和による**「4次元格子多面体の整数点カウント問題」**へと代数的に射影（ACX Jump）した点です。さらに、その格子点数がエルハート多項式（Ehrhart Polynomial）に従うという上位の数学的真理（Ultimate Truth）を利用し、未知の領域を数点の初期データからのラグランジュ補間によって $O(1)$ で現成させた構造は、まさに真の中道と言えます。
||#

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
g(16) = 234897386493229284

User time    =        0.000
System time  =        0.000
Elapsed time =        0.004
Allocation   = 200 bytes
43 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 234897386493229284
:ok