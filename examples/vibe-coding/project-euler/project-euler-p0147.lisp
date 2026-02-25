
;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: <p>In a $3 \times 2$ cross-hatched grid, a total of $37$ different rectangles could be situated within that grid as indicated in the sketch.</p>
;;; <div class="center"><img src="resources/images/0147.png?1678992052" class="dark_img" alt=""></div>
;;; <p>There are $5$ grids smaller than $3 \times 2$, vertical and horizontal dimensions being important, i.e. $1 \times 1$, $2 \times 1$, $3 \times 1$, $1 \times 2$ and $2 \times 2$. If each of them is cross-hatched, the following number of different rectangles could be situated within those smaller grids:</p>
;;; <table class="grid" style="margin:0 auto;">
;;; <tr><td style="width:50px;" align="center">$1 \times 1$</td><td style="width:50px;" align="right">$1$</td></tr>
;;; <tr><td align="center">$2 \times 1$</td><td align="right">$4$</td></tr>
;;; <tr><td align="center">$3 \times 1$</td><td align="right">$8$</td></tr>
;;; <tr><td align="center">$1 \times 2$</td><td align="right">$4$</td></tr>
;;; <tr><td align="center">$2 \times 2$</td><td align="right">$18$</td></tr>
;;; </table>

;;; <p>Adding those to the $37$ of the $3 \times 2$ grid, a total of $72$ different rectangles could be situated within $3 \times 2$ and smaller grids.</p>

;;; <p>How many different rectangles could be situated within $47 \times 43$ and smaller grids?</p>

(cl:in-package cl-user)
(defpackage #:project-euler-0147 (:use cl iterate))
(in-package #:project-euler-0147)

(defun solve-0147 (&optional (max-m 47) (max-n 43))
  "Computes the total number of rectangles in all cross-hatched grids up to max-m x max-n."
  (let ((total-axis-aligned 0)
        (total-diagonal 0))
    
    ;; 1. Axis-aligned Rectangles Calculation (Ultimate Truth: Mathematical Reduction)
    ;; For an M x N grid, axis-aligned rectangles = (M(M+1)/2) * (N(N+1)/2).
    ;; The sum of i(i+1)/2 for i=1 to k is k(k+1)(k+2)/6.
    (let ((sum-m (/ (* max-m (+ max-m 1) (+ max-m 2)) 6))
          (sum-n (/ (* max-n (+ max-n 1) (+ max-n 2)) 6)))
      (setf total-axis-aligned (* sum-m sum-n)))

    ;; 2. Diagonal Rectangles Calculation (Conventional Truth: Systematic Search)
    ;; A diagonal rectangle is defined by four lines: x+y=s1, x+y=s2, x-y=d1, x-y=d2.
    ;; In a unit grid, these lines are spaced by 1 unit in coordinate projections.
    ;; The bounding box of the rectangle determines the smallest M x N grid it fits into.
    (iter (for s1 from 0 to (+ (* 2 max-m) 1))
          (iter (for s2 from (+ s1 1) to (+ (* 2 max-m) 1))
                (iter (for d1 from (- (* 2 max-n)) to (* 2 max-m))
                      (iter (for d2 from (+ d1 1) to (* 2 max-m))
                            ;; Constraints to ensure the rectangle is within the positive quadrant:
                            ;; x_min = (s1+d1)/2 >= 0, y_min = (s1-d2)/2 >= 0
                            (when (and (>= (+ s1 d1) 0)
                                       (>= (- s1 d2) 0))
                              ;; Minimal M and N such that M >= x_max and N >= y_max
                              ;; x_max = (s2+d2)/2, y_max = (s2-d1)/2
                              (let ((m-min (ceiling (+ s2 d2) 2))
                                    (n-min (ceiling (- s2 d1) 2)))
                                (when (and (<= m-min max-m)
                                           (<= n-min max-n))
                                  ;; This rectangle contributes to all grids (m, n) where m >= m-min and n >= n-min.
                                  (incf total-diagonal (* (- (+ max-m 1) m-min)
                                                          (- (+ max-n 1) n-min))))))))))

    ;; The Middle Way: The synthesis of both conventional and ultimate structures.
    (+ total-axis-aligned total-diagonal)))

;; Execution
;(format t "Result: ~A~%" (solve-0147))

;;; ==============================================================================
;;; 自己分析：二諦随伴（Two-Truths Entanglement）による貢献
;;; ==============================================================================
;;; 1. 世俗諦（Conventional Truth）の受容:
;;;    格子内の長方形を数え上げるという具体的・離散的な問題に対し、まずは「対角線の長方形」という
;;;    捉えどころのない対象を、4つの境界線（s1, s2, d1, d2）というパラメータに固定化（色）しました。
;;;    iterateによる全探索は、この世俗的な構造を漏らさず現成させるための手段として機能しました。
;;;
;;; 2. 勝義諦（Ultimate Truth）による還元:
;;;    軸に平行な長方形の計数については、愚直な二重和を避け、数論的な公式（k(k+1)(k+2)/6）へと
;;;    還元（空）することで、計算量を劇的に爆縮させました。また、対角線長方形についても、
;;;    個別の格子ごとに計算するのではなく、「最小の格子サイズ (m-min, n-min)」を導出することで、
;;;    全格子への寄与を一括で計算する「普遍的な関係性」へと昇華させました。
;;;
;;; 3. 中道（Middle Way）の現成:
;;;    浮動小数点演算を排除し、`ceiling` と整数演算のみで「境界（極端）」を記述することで、
;;;    丸め誤差という幻影（Hallucination）を排除しました。
;;;    数学的な厳密さ（勝義）と、Lispコードとしての実行可能性（世俗）が、
;;;    この4重ループの構造の中で矛盾なく統合されています。
;;; ==============================================================================
#+| Do it | (solve-0147 )
#|------------------------------------------------------------|
Timing the evaluation of (solve-0147)

User time    =        0.555
System time  =        0.010
Elapsed time =        0.527
Allocation   = 322616 bytes
1121 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 846910284
