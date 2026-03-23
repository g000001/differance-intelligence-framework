;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0651 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0651)

#||
(cl:comment "CLIF representation of PE 651 mathematical invariants and symmetry reductions")
(cl:text
  ;; 1. The total symmetry group of the finite periodic cylinder is the direct product of Dihedral groups.
  (forall (a b)
    (= (symmetry-group a b) 
       (direct-product (dihedral-group (* 2 a)) (dihedral-group (* 2 b)))))

  ;; 2. Burnside's Lemma counts patterns with AT MOST k colors.
  (forall (k a b)
    (= (g k a b)
       (* (inverse (size (symmetry-group a b)))
          (sum (c (cycles (symmetry-group a b)))
               (* (weight c) (expt k c))))))

  ;; 3. Principle of Inclusion-Exclusion extracts EXACTLY m colors.
  (forall (m a b)
    (= (f m a b)
       (sum (k 1 m)
            (* (expt -1 (- m k))
               (choose m k)
               (g k a b)))))

  ;; 4. Number-theoretic shortcut: Symmetries are grouped by GCD to avoid O(N) iteration.
  (forall (n shift)
    (= (cycle-structure (rotation shift n))
       (cycle-structure (rotation (gcd shift n) n))))
)
||#

(defconstant $modulo 1000000007)

(defun get-divisors (n)
  "O(sqrt(N)) で約数を取得します。"
  (let ((divs '()))
    (iterate (for i from 1 to (isqrt n))
             (when (= (mod n i) 0)
               (push i divs)
               (when (/= i (/ n i))
                 (push (/ n i) divs))))
    divs))

(defun euler-phi (n)
  "O(sqrt(N)) でオイラーのトーティエント関数を計算します。"
  (let ((res n)
        (p 2)
        (current-n n))
    (iterate (while (<= (* p p) current-n))
             (when (= (mod current-n p) 0)
               (setf res (- res (/ res p)))
               (iterate (while (= (mod current-n p) 0))
                        (setf current-n (/ current-n p))))
             (incf p))
    (when (> current-n 1)
      (setf res (- res (/ res current-n))))
    res))

(defun build-cycle-structure (n)
  "O(d(N)) で Dihedral Group D_2n の巡回構造（同値類）を構築し、最悪ケース O(N) の走査を回避します。"
  (let ((cs '()))
    ;; 回転群 (Rotations)
    ;; n個の要素をシフトさせる操作。gcd(shift, n) = k となるシフトは phi(n/k) 個存在する。
    (dolist (d (get-divisors n))
      (let ((k d))
        (push (list (euler-phi (/ n k)) (list (list (/ n k) k))) cs)))
    
    ;; 鏡映群 (Reflections)
    (if (oddp n)
        (push (list n (list (list 1 1) (list 2 (floor n 2)))) cs)
        (progn
          (push (list (/ n 2) (list (list 1 2) (list 2 (/ (- n 2) 2)))) cs)
          (push (list (/ n 2) (list (list 2 (/ n 2)))) cs)))
    cs))

(defun mod-pow (base exp)
  "フェルマーの小定理に基づく高速累乗"
  (let ((res 1)
        (b (mod base $modulo)))
    (iterate (while (> exp 0))
             (when (oddp exp)
               (setf res (mod (* res b) $modulo)))
             (setf b (mod (* b b) $modulo))
             (setf exp (ash exp -1)))
    res))

(defun mod-inv (n)
  "モジュラ逆数。$moduloが素数であるためフェルマーの小定理を適用。"
  (mod-pow n (- $modulo 2)))

(defun mod-comb (n k)
  "二項係数 nCk mod $modulo"
  (if (or (< k 0) (> k n))
      0
      (let ((num 1)
            (den 1))
        (iterate (for i from 1 to k)
                 (setf num (mod (* num (- (+ n 1) i)) $modulo))
                 (setf den (mod (* den i) $modulo)))
        (mod (* num (mod-inv den)) $modulo))))

(defun get-fibonacci (n)
  "F_0 = 0, F_1 = 1 から始まるフィボナッチ数列"
  (let ((a 0) (b 1))
    (iterate (for i from 0 below n)
             (let ((next (+ a b)))
               (setf a b)
               (setf b next)))
    a))

(defun compute-f (m a b)
  "Burnsideの補題と包除原理を用いて f(m, a, b) を計算します。"
  (let ((cs-a (build-cycle-structure a))
        (cs-b (build-cycle-structure b))
        (c-counts (make-hash-table :test 'eql)))
    
    ;; 2つのDihedral Groupの直積から生成される巡回群の数をフェルミ推定通りに O(d(a)*d(b)) で集計
    (iterate (for item-a in cs-a)
             (let ((wa (first item-a))
                   (cyca (second item-a)))
               (iterate (for item-b in cs-b)
                        (let ((wb (first item-b))
                              (cycb (second item-b)))
                          (let ((c-total 0))
                            (iterate (for ca in cyca)
                                     (let ((la (first ca))
                                           (count-a (second ca)))
                                       (iterate (for cb in cycb)
                                                (let ((lb (first cb))
                                                      (count-b (second cb)))
                                                  ;; 直積の性質により、長さ la と lb の巡回群が交わると
                                                  ;; gcd(la, lb) 個のサイクルが生成される
                                                  (incf c-total (* count-a count-b (gcd la lb)))))))
                            (let ((weight (mod (* wa wb) $modulo)))
                              (incf (gethash c-total c-counts 0) weight)
                              (setf (gethash c-total c-counts) (mod (gethash c-total c-counts) $modulo))))))))
    
    ;; g(k): "最大で" k 色を使うパターンの数 (Burnside)
    (let ((g-vals (make-array (1+ m) :initial-element 0))
          (inv-4ab (mod-inv (mod (* 4 a b) $modulo))))
      (iterate (for k from 1 to m)
               (let ((sum 0))
                 (iterate (for (c weight) in-hashtable c-counts)
                          (setf sum (mod (+ sum (* weight (mod-pow k c))) $modulo)))
                 (setf (aref g-vals k) (mod (* sum inv-4ab) $modulo))))
      
      ;; f(m): 包除原理で "ちょうど" m 色使うパターンを抽出
      (let ((f-val 0))
        (iterate (for k from 1 to m)
                 (let ((term (mod (* (mod-comb m k) (aref g-vals k)) $modulo)))
                   (if (oddp (- m k))
                       (setf f-val (mod (- f-val term) $modulo))
                       (setf f-val (mod (+ f-val term) $modulo)))))
        f-val))))

(defun solve ()
  (let ((total-sum 0))
    (format t "--- Project Euler 651 Execution Started ---~%")
    (iterate (for i from 4 to 40)
             (let ((a (get-fibonacci (- i 1)))
                   (b (get-fibonacci i)))
               (let ((f-val (compute-f i a b)))
                 (format t "i = ~2D | a = ~10D | b = ~10D | f(~D) = ~10D~%" i a b i f-val)
                 (setf total-sum (mod (+ total-sum f-val) $modulo)))))
    (format t "Final Result Sum: ~D~%" total-sum)
    total-sum))



#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
--- Project Euler 651 Execution Started ---
i =  4 | a =          2 | b =          3 | f(4) =        136
i =  5 | a =          3 | b =          5 | f(5) =  421890004
i =  6 | a =          5 | b =          8 | f(6) =  825396705
i =  7 | a =          8 | b =         13 | f(7) =  612760586
i =  8 | a =         13 | b =         21 | f(8) =   49718354
i =  9 | a =         21 | b =         34 | f(9) =  585501454
i = 10 | a =         34 | b =         55 | f(10) =  859541562
i = 11 | a =         55 | b =         89 | f(11) =  366169568
i = 12 | a =         89 | b =        144 | f(12) =  753459196
i = 13 | a =        144 | b =        233 | f(13) =  907081451
i = 14 | a =        233 | b =        377 | f(14) =  556998801
i = 15 | a =        377 | b =        610 | f(15) =  154058006
i = 16 | a =        610 | b =        987 | f(16) =   39567146
i = 17 | a =        987 | b =       1597 | f(17) =  167067899
i = 18 | a =       1597 | b =       2584 | f(18) =  455263162
i = 19 | a =       2584 | b =       4181 | f(19) =  657447269
i = 20 | a =       4181 | b =       6765 | f(20) =  238732219
i = 21 | a =       6765 | b =      10946 | f(21) =  327321741
i = 22 | a =      10946 | b =      17711 | f(22) =  347511859
i = 23 | a =      17711 | b =      28657 | f(23) =  556709634
i = 24 | a =      28657 | b =      46368 | f(24) =  138657397
i = 25 | a =      46368 | b =      75025 | f(25) =  554190827
i = 26 | a =      75025 | b =     121393 | f(26) =  276770228
i = 27 | a =     121393 | b =     196418 | f(27) =  623041651
i = 28 | a =     196418 | b =     317811 | f(28) =  864159536
i = 29 | a =     317811 | b =     514229 | f(29) =  205948607
i = 30 | a =     514229 | b =     832040 | f(30) =  124543747
i = 31 | a =     832040 | b =    1346269 | f(31) =  401998805
i = 32 | a =    1346269 | b =    2178309 | f(32) =  282787607
i = 33 | a =    2178309 | b =    3524578 | f(33) =  673575971
i = 34 | a =    3524578 | b =    5702887 | f(34) =  917835585
i = 35 | a =    5702887 | b =    9227465 | f(35) =  214269610
i = 36 | a =    9227465 | b =   14930352 | f(36) =  832205418
i = 37 | a =   14930352 | b =   24157817 | f(37) =   62151599
i = 38 | a =   24157817 | b =   39088169 | f(38) =  286667041
i = 39 | a =   39088169 | b =   63245986 | f(39) =  941760324
i = 40 | a =   63245986 | b =  102334155 | f(40) =  165472558
Final Result Sum: 448233151

User time    =        0.196
System time  =        0.012
Elapsed time =        0.157
Allocation   = 731912 bytes
3708 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 448233151
:ok