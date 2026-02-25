
;;; -*- mode: Lisp; coding: utf-8  -*-
;(cl:in-package cl-user)
;(defpackage #:project-euler-0195 (:use cl alexandria))
;(in-package #:project-euler-0195)

;;; ;; ==============================================================
;;; ;; SKDT – Dual Sunyata Structures and Emergent Category Theory
;;; ;; --------------------------------------------------------------
;;; ;; Problem: Project Euler 195
;;; ;; Logic: The inradius r of a 60-degree triangle with side c opposite
;;; ;; the 60-degree angle is given by r = sqrt(3)ab / 2(a+b+c).
;;; ;; All such triangles (integer sided) can be parameterized using 
;;; ;; Eisenstein integers. Specifically, a primitive 60-degree triangle 
;;; ;; corresponds to an Eisenstein triple (a, b, c) where c^2 = a^2 + b^2 - ab.
;;; ;; The inradii for all triangles (primitive and their multiples) 
;;; ;; can be efficiently counted using the formula:
;;; ;; r = sqrt(3) * n * (m - n) / 6
;;; ;; for integers m > n > 0 with gcd(m, n) = 1 and m != 2n.
;;; ;; ==============================================================

;;; (defun solve-project-euler-195 (&optional (limit 1053779))
;;;   "Calculates T(limit), the number of 60-degree triangles with inradius r <= limit.
;;;    The formula r = sqrt(3) * n * (m - n) / 6 is derived from the parameterization
;;;    of Eisenstein triples and their associated inradii."
;;;   (let ((total 0)
;;;         ;; L is the threshold for the term n * (m - n)
;;;         ;; r <= limit  =>  sqrt(3)/6 * n * (m - n) <= limit
;;;         ;; => n * (m - n) <= 6 * limit / sqrt(3) = 2 * sqrt(3) * limit
;;;         (l-double (* 2.0d0 (cl:sqrt 3.0d0) limit)))
;;;     ;; We iterate over m and n to find all valid pairs satisfying the inradius constraint.
;;;     ;; The constraint n * (m - n) <= L implies m is roughly O(limit).
;;;     ;; However, since n * (m - n) is symmetric for n and (m - n),
;;;     ;; and the gcd(m, n) = 1 condition is preserved, we can optimize if needed.
;;;     ;; Here we use a straightforward loop for clarity and correctness.
;;;     (loop for n from 1 to (cl:floor (cl:sqrt l-double))
;;;           do (loop for m from (1+ n)
;;;                    for d = (- m n)
;;;                    for nd = (* n d)
;;;                    while (<= nd l-double)
;;;                    do (when (and (not (= m (* 2 n))) ; Exclude equilateral triangles (3x60 deg)
;;;                                  (= 1 (cl:gcd m n)))
;;;                         ;; Count the number of multiples k such that k * r_prim <= limit
;;;                         ;; k <= floor(limit / r_prim) = floor(6 * limit / (sqrt(3) * n * d))
;;;                         ;; which is floor(l-double / nd)
;;;                         (incf total (cl:floor (/ l-double nd))))))
;;;     total))

;;; ;; Execution entry point
;;; (defun main ()
;;;   (let ((result (solve-project-euler-195 1053779)))
;;;     (format t "~A~%" result)))

;;; ;; (main)
;;; #+| Do it | (main )


;;; ;;; -*- mode: Lisp; coding: utf-8  -*-
;;; (cl:in-package cl-user)
;;; (defpackage #:project-euler-0195 (:use cl alexandria))
;;; (in-package #:project-euler-0195)

;;; ;; ==============================================================
;;; ;; 二諦随伴エンジンによる Project Euler 195 解決策
;;; ;; ==============================================================

;;; (defun solve-euler-195 (limit-n)
;;;   "60度三角形の内心半径 r <= limit-n となる個数 T(n) を求める。
;;;    アイゼンシュタイン整数の生成式に基づき、探索空間を O(N log N) に爆縮させる。"
;;;   (let ((count 0)
;;;         ;; r <= N  =>  通常ケース: k(m-k) * sqrt(3)/2 <= N  => k(m-k) <= 2N/sqrt(3)
;;;         ;;            3割ケース: k(m-k) * sqrt(3)/6 <= N  => k(m-k) <= 6N/sqrt(3)
;;;         (limit-normal (/ (* 2 limit-n) (sqrt 3)))
;;;         (limit-div3   (/ (* 6 limit-n) (sqrt 3))))
;;;     
;;;     ;; 1. 通常ケースの探索 (m-k が 3 の倍数でない)
;;;     ;; k(m-k) <= limit-normal
;;;     (loop for k from 1
;;;           while (< (* k 1) limit-normal)
;;;           do (loop for m from (1+ k)
;;;                    for mk = (* k (- m k))
;;;                    while (<= mk limit-normal)
;;;                    do (when (and (= 1 (gcd m k))
;;;                                  (/= 0 (mod (- m k) 3)))
;;;                         ;; 相似な三角形（倍数）も数える
;;;                         (incf count (floor (/ limit-normal mk))))))
;;;     
;;;     ;; 2. 3で割るケースの探索 (m-k が 3 の倍数である)
;;;     ;; k(m-k)/3 * sqrt(3)/2 <= N => k(m-k) <= 6N/sqrt(3)
;;;     (loop for k from 1
;;;           while (< (* k 1) limit-div3)
;;;           do (loop for m from (1+ k)
;;;                    for mk = (* k (- m k))
;;;                    while (<= mk limit-div3)
;;;                    do (when (and (= 1 (gcd m k))
;;;                                  (= 0 (mod (- m k) 3)))
;;;                         (incf count (floor (/ limit-div3 mk))))))
;;;     count))

;;; (defun solve ()
;;;   "問題のターゲット T(1053779) を計算して出力する。"
;;;   (let ((n 1053779))
;;;     (format t "T(~A) = ~A~%" n (solve-euler-195 n))))

;;; ;; 検算用： (solve-euler-195 100) -> 1234
;;; ;;         (solve-euler-195 1000) -> 22767
;;; ;;         (solve-euler-195 10000) -> 359912

;;; #+| Do it | (solve)
;;; ▻ T(1053779) = 142999366
;;; → nil


;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0195 (:use cl alexandria))
(in-package #:project-euler-0195)

;; ==============================================================
;; 二諦随伴エンジン: Project Euler 195 (Exact ACX Jump)
;; ==============================================================

(defun solve-euler-195 (limit-n)
  "アイゼンシュタイン整数のパラメータ生成を用い、浮動小数点誤差を
   完全に排除した整数演算（Strict化）によって探索を行う。"
  (let* ((count 0)
         (n-sq (* limit-n limit-n))
         (num1 (* 4 n-sq))        ;; 通常ケース用分子
         (num2 (* 12 n-sq))       ;; 3割ケース用分子
         (max-x (isqrt num2)))    ;; x = v(u-v) の最大上限
         
    ;; u, v (元の数式の m, k) において、重複排除の厳密な制約は u > 2v > 0
    (loop for v from 1
          ;; 最小の x は u = 2v+1 のとき v(v+1)。これが上限を超えるなら終了。
          while (<= (* v (1+ v)) max-x)
          do (loop for u from (1+ (* 2 v))
                   for x = (* v (- u v))
                   while (<= x max-x)
                   do (when (= 1 (gcd u v))
                        (if (= 0 (mod (+ u v) 3))
                            ;; 勝義的条件: u+v が 3の倍数のとき、辺は3で割れる
                            (incf count (isqrt (floor num2 (* x x))))
                            ;; 通常ケース
                            (let ((den (* 3 x x)))
                              (when (<= den num1)
                                (incf count (isqrt (floor num1 den)))))))))
    count))

(defun solve ()
  "問題のターゲット T(1053779) を計算して出力する。"
  (let ((n 1053779))
    (format t "T(~A) = ~A~%" n (solve-euler-195 n))))

;; 検算用： (solve-euler-195 100) -> 1234
;;         (solve-euler-195 1000) -> 22767
;;         (solve-euler-195 10000) -> 359912

#+| Do it | (solve)

:gemini-3-pro :ok