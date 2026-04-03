;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0520 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0520)

#||
【数学的考察と次元崩壊の構築】
1. 条件の分析と指数型母関数 (EGF):
   "simber" の条件は、各数字の出現回数に以下のパリティ制約を課す。
   - 偶数 (0, 2, 4, 6, 8): 偶数回 (0回を含む)。EGF = cosh(x)
   - 奇数 (1, 3, 5, 7, 9): 0回、または奇数回。EGF = 1 + sinh(x)
   先頭のゼロを許容した場合、長さ L の文字列の数は L! [x^L] F(x) となる。
   F(x) = (cosh x)^5 * (1 + sinh x)^5
   
2. 先頭ゼロの排除:
   先頭がゼロから始まる長さ L の文字列は、残りの L-1 文字において
   「0 が奇数回出現する (全体の 0 の数が偶数になるため)」という条件に反転する。
   よって、除外すべき文字列の数は (L-1)! [x^{L-1}] G(x) となる。
   G(x) = sinh(x) * (cosh x)^4 * (1 + sinh x)^5

3. 多項式へのマッピングと O(1) への次元崩壊:
   e^x = y と置換すると、F(x), G(x) は y の多項式に変換される。
   cosh x = (y + y^{-1})/2, sinh x = (y - y^{-1})/2 より、
   F(y) = (y^2+1)^5 (y^2+2y-1)^5 / (1024 y^{10})
   G(y) = (y^2-1)(y^2+1)^4 (y^2+2y-1)^5 / (1024 y^{10})
   これらの分子を多項式展開し、係数を c_k, d_k (0 <= k <= 20) とすると、
   j = k - 10 (-10 <= j <= 10) として、長さ L の正しい simber の数 U(L) は
   U(L) = (1/1024) * \sum_{j=-10}^{10} (c_{j+10} j^L - d_{j+10} j^{L-1})
   という 21項の定数和に崩壊する。

4. 累積和 Q(N) の高速計算:
   Q(N) = \sum_{L=1}^N U(L) は、j^L の等比数列の和 S_1(j, N) と S_0(j, N) に分解できる。
   S_1(j, N) = j (j^N - 1)/(j - 1) mod M
   S_0(j, N) = (j^N - 1)/(j - 1) mod M
   これにより、N = 2^{39} という天文学的数字であっても、O(log N) のモジュラ累乗で計算可能となる。
||#

(defconstant +MOD+ 1000000123)

(defvar *C-poly* nil)
(defvar *D-poly* nil)

(defun poly-mul (p1 p2)
  "2つの多項式を掛け合わせる"
  (let* ((len1 (length p1))
         (len2 (length p2))
         (res (make-array (+ len1 len2 -1) :initial-element 0)))
    (iterate (for i from 0 below len1)
      (iterate (for j from 0 below len2)
        (incf (aref res (+ i j)) (* (aref p1 i) (aref p2 j)))))
    res))

(defun init-polys ()
  "EGFを展開した多項式係数を事前計算する"
  (let* ((A #(1 0 1))        ;; 1 + y^2
         (B #(-1 2 1))       ;; -1 + 2y + y^2
         (A2 (poly-mul A A))
         (A4 (poly-mul A2 A2))
         (A5 (poly-mul A4 A))
         (B2 (poly-mul B B))
         (B4 (poly-mul B2 B2))
         (B5 (poly-mul B4 B))
         (y2-1 #(-1 0 1)))   ;; -1 + y^2
    (setf *C-poly* (poly-mul A5 B5))
    (setf *D-poly* (poly-mul y2-1 (poly-mul A4 B5)))))

(defun power-mod (base exp)
  "モジュラ累乗計算"
  (let ((res 1)
        (b (mod base +MOD+))
        (e exp))
    (iterate (while (> e 0))
      (when (oddp e)
        (setf res (mod (* res b) +MOD+)))
      (setf b (mod (* b b) +MOD+))
      (setf e (ash e -1)))
    res))

(defun compute-Q (N)
  "等比数列の和を用いて O(1) で Q(N) mod M を計算する"
  (let ((ans 0)
        (inv1024 (power-mod 1024 (- +MOD+ 2))))
    (iterate (for k from 0 to 20)
      (let* ((j (- k 10))
             (cj (aref *C-poly* k))
             (dj (aref *D-poly* k))
             (s1 0)
             (s0 0))
        (cond
          ((= j 0)
           (setf s1 0)
           (setf s0 1))
          ((= j 1)
           (setf s1 (mod N +MOD+))
           (setf s0 (mod N +MOD+)))
          (t
           (let* ((j-mod (mod j +MOD+))
                  (num (mod (- (power-mod j-mod N) 1) +MOD+))
                  (den (mod (- j-mod 1) +MOD+))
                  (inv-den (power-mod den (- +MOD+ 2))))
             (setf s0 (mod (* num inv-den) +MOD+))
             (setf s1 (mod (* j-mod s0) +MOD+)))))
        
        (let* ((term1 (mod (* (mod cj +MOD+) s1) +MOD+))
               (term2 (mod (* (mod dj +MOD+) s0) +MOD+))
               (val (mod (- term1 term2) +MOD+)))
          (setf ans (mod (+ ans val) +MOD+)))))
    (mod (* ans inv1024) +MOD+)))

(defun solve ()
  (format t "観測: 母関数に基づく多項式係数を構築中...~%")
  (init-polys)
  
  (format t "観測: テストケース Q(7) を検証中...~%")
  (let ((ans7 (compute-Q 7)))
    (format t "観測: Q(7) = ~D (Expected: 287975)~%" ans7))
    
  (format t "観測: テストケース Q(100) を検証中...~%")
  (let ((ans100 (compute-Q 100)))
    (format t "観測: Q(100) mod ~D = ~D (Expected: 123864868)~%" +MOD+ ans100))
    
  (format t "観測: 本探索 Sum_{1<=u<=39} Q(2^u) を実行中...~%")
  (let ((ans 0))
    (iterate (for u from 1 to 39)
      (let ((N (ash 1 u)))
        (setf ans (mod (+ ans (compute-Q N)) +MOD+))))
    (format t "Answer: ~D~%" ans)
    ans))

#+| Do it | (project-euler-0520:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: 母関数に基づく多項式係数を構築中...
観測: テストケース Q(7) を検証中...
観測: Q(7) = 287975 (Expected: 287975)
観測: テストケース Q(100) を検証中...
観測: Q(100) mod 1000000123 = 123864868 (Expected: 123864868)
観測: 本探索 Sum_{1<=u<=39} Q(2^u) を実行中...
Answer: 238413705

User time    =        0.002
System time  =        0.000
Elapsed time =        0.001
Allocation   = 1640 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 238413705
:ok
