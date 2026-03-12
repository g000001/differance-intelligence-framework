;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0270 (:use cl iterate alexandria))
(in-package #:project-euler-0270)

#||
(cl-text euler-acx-p270-arx-core
  (cl-comment "[COMMAND: ARX-CORE-RESET]")
  (cl-comment "L1=Silence; Seed=Catalan_Generating_Function_Composition; Goal=AC_Minimization; Output=Alethetic_Normal_Form;")

  (cl-comment "=== Project Euler 270: Alethetic Reset (Generating Function Projection) ===")
  (cl-comment "Simulating the cuts or using DP on a grid is Dukkha (illusion) and scales horribly,")
  (cl-comment "as the state space of arbitrary polygonal regions is O(2^{4N}).")
  (cl-comment "We map the geometric cutting process to the triangulation of a 4N-gon.")
  (cl-comment "A maximal valid set of cuts corresponds EXACTLY to a triangulation of the 4N-gon")
  (cl-comment "where NO internal diagonal connects two vertices on the same side of the square.")
  
  (cl-comment "Let Tri(x, y, z, w) be the generating function of ALL triangulations of a polygon")
  (cl-comment "with its perimeter partitioned into 4 paths. Tri(x,y,z,w) = Sum C_{a+b+c+d-2} x^a y^b z^c w^d.")
  (cl-comment "Let F(X, Y, Z, W) be the generating function of 'core' triangulations (valid square cuts).")
  (cl-comment "Because any generic triangulation is formed by taking a core triangulation and expanding")
  (cl-comment "its boundary edges into triangulations of smaller polygons on the same side, we have:")
  (forall (x y z w)
    (Equal (Tri x y z w) (F (T x) (T y) (T z) (T w))))
    
  (cl-comment "Where T(x) = x + T(x)^2 is the Catalan generating function.")
  (cl-comment "Since x = T(x) - T(x)^2, we can formally invert this substitution:")
  (forall (X Y Z W)
    (Equal (F X Y Z W) (Tri (- X (^ X 2)) (- Y (^ Y 2)) (- Z (^ Z 2)) (- W (^ W 2)))))

  (cl-comment "We need the coefficient of X^N Y^N Z^N W^N in F, which is exactly the coefficient")
  (cl-comment "of X^N Y^N Z^N W^N in Tri(X-X^2, Y-Y^2, Z-Z^2, W-W^2).")
  (cl-comment "By defining P(X) as the terms of X^N in (X-X^2)^a, the answer is derived from P(X)^4.")
  (cl-comment "This leap completely annihilates the O(2^{4N}) combinatorial debt into a pure O(N^2) polynomial multiplication.")
)
||#

(defun binomial (n k)
  "Computes binomial coefficient exactly."
  (if (or (< k 0) (> k n))
      0
      (let ((k-min (min k (- n k)))
            (res 1))
        (iterate (for i from 1 to k-min)
          (setf res (floor (* res (- (1+ n) i)) i)))
        res)))

(defun catalan (n)
  "Computes the n-th Catalan number."
  (if (< n 0)
      0
      (floor (binomial (* 2 n) n) (1+ n))))

(defun poly-mul (p1 p2)
  "Multiplies two polynomials represented as arrays."
  (let* ((len1 (length p1))
         (len2 (length p2))
         (res (make-array (+ len1 len2 -1) :initial-element 0 :element-type 'integer)))
    (iterate (for i from 0 below len1)
      (let ((c1 (aref p1 i)))
        (when (not (zerop c1))
          (iterate (for j from 0 below len2)
            (incf (aref res (+ i j)) (* c1 (aref p2 j)))))))
    res))

(defun build-P (n)
  "Builds the polynomial P(X) whose terms map to the (X-X^2)^a expansion for a single side."
  (let ((p (make-array (1+ n) :initial-element 0 :element-type 'integer)))
    ;; The lowest possible power is ceiling(N/2)
    (iterate (for a from (floor (1+ n) 2) to n)
      (let* ((n-a (- n a))
             (term (binomial a n-a)))
        ;; Apply the sign from (-X^2)^{N-a}
        (when (oddp n-a)
          (setf term (- term)))
        (setf (aref p a) term)))
    p))

(defun solve-270 (&optional (n 30))
  "Computes C(30) mod 10^8 in O(N^2) using Generating Functions."
  (let* ((p1 (build-P n))
         (p2 (poly-mul p1 p1))
         (p4 (poly-mul p2 p2)) ; P(X)^4 representing all 4 sides combined
         (ans 0))
    ;; The coefficient of the combined term of degree A is multiplied by Catalan(A-2)
    (iterate (for a from 0 below (length p4))
      (let ((c (aref p4 a)))
        (when (not (zerop c))
          (incf ans (* c (catalan (- a 2)))))))
    ;; Return the result modulo 10^8
    (mod ans 100000000)))

;;; ============================================================================
;;; 自己分析 (Self-Analysis)
;;; ============================================================================
;;; 
;;; 1. 現実的な時間での終了可能性について
;;; 本アルゴリズムは O(2^{4N}) 要求されるような複雑な状態遷移を完全に放棄し、
;;; 最大次数 4N (N=30 のとき 120) の多項式乗算に還元しています。
;;; `poly-mul` は O(N^2) であり、サイズ 31, 61 の配列を掛け合わせるだけなので、
;;; 全体の演算回数はわずか数千回です。120次程度のカタラン数も Lisp の多倍長整数（Bignum）
;;; を用いて瞬時に計算されるため、実行時間は 1ミリ秒未満（0.001秒以下）となり、
;;; 予測時間内で確実に終了します。無限ループの懸念は一切ありません。
;;;
;;; 2. LLMが陥りやすい罠
;;; LLMは「図形を直線で切断する」という問題を見ると、計算幾何学的なアプローチ
;;; （直線の交差判定や、新しく生まれた多角形を再帰的に DP で分割していく手法）に
;;; 強烈に引き込まれる「悪取空」の傾向があります。
;;; この罠にはまると、生成される多角形の形状が無限に分岐するため状態空間が爆発し、
;;; メモリ枯渇とタイムアウトで自滅します。
;;;
;;; 3. 問題文に含まれていた計算量削減のための制約について
;;; 問題文中の「交差しない直線で切る」「境界点で交わってもよい」「これ以上切れなくなるまで続ける」
;;; というルールの本質は、「4N個の境界点からなる凸多角形の『完全な三角分割』」です。
;;; かつ、「同じ辺の上にある2点間を切れない」という制約のみが課されています。
;;; この図形的な制約を母関数の代数的操作へと転写できることが、出題者が用意した最大の「抜け道」です。
;;;
;;; 4. 発明や創発
;;; GA（遺伝的アルゴリズム）や枝刈り探索などを一切使わず、純粋な数学的創発（ACX Jump）を行いました。
;;; 任意の三角分割の母関数 `Tri` と、問題が要求する制約を満たす三角分割の母関数 `F` との間に
;;; `Tri(x) = F(T(x))` (Tはカタラン母関数) という代数的なフラクタル関係を見出した点です。
;;; さらに `x = T(x) - T(x)^2` の逆変換を利用することで、求めるべき `F` の係数を、
;;; 単純な多項式 `P(x)^4` の展開係数へと「裏返し（一括射影）」で抽出したことが最大のブレイクスルーです。

#+| Do it | (solve-270 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-270)

User time    =        0.001
System time  =        0.000
Elapsed time =        0.001
Allocation   = 375992 bytes
72 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 82282080
