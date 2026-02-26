;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: <p>The minimum number of cubes to cover every visible face on a cuboid measuring $3 \times 2 \times 1$ is twenty-two.</p>
;;; <div class="center">
;;; <img src="resources/images/0126.png?1678992052" class="dark_img" alt=""><br></div>
;;; <p>If we then add a second layer to this solid it would require forty-six cubes to cover every visible face, the third layer would require seventy-eight cubes, and the fourth layer would require one-hundred and eighteen cubes to cover every visible face.</p>
;;; <p>However, the first layer on a cuboid measuring $5 \times 1 \times 1$ also requires twenty-two cubes; similarly the first layer on cuboids measuring $5 \times 3 \times 1$, $7 \times 2 \times 1$, and $11 \times 1 \times 1$ all contain forty-six cubes.</p>
;;; <p>We shall define $C(n)$ to represent the number of cuboids that contain $n$ cubes in one of its layers. So $C(22) = 2$, $C(46) = 4$, $C(78) = 5$, and $C(118) = 8$.</p>
;;; <p>It turns out that $154$ is the least value of $n$ for which $C(n) = 10$.</p>
;;; <p>Find the least value of $n$ for which $C(n) = 1000$.</p>


(cl:in-package cl-user)
(defpackage #:project-euler-0126 (:use cl #:iterate))
(in-package #:project-euler-0126)

(defun solve-0126 (&optional (target-count 1000))
  "Finds the least n such that C(n) = target-count."
  ;; 1. Axiomatic Grounding: 
  ;; The number of cubes in the k-th layer of an a x b x c cuboid is given by:
  ;; n = 2(ab + bc + ca) + 4(a + b + c + k - 2)(k - 1)
  ;; We must find the least n where this formula has exactly 1000 integer solutions
  ;; for (a, b, c, k) such that a >= b >= c >= 1 and k >= 1.
  
  (let* ((limit 20000) ; Initial heuristic limit based on C(154)=10
         (counts (make-array (1+ limit) :element-type 'fixnum :initial-element 0)))
    
    ;; 2. Bijective Generation:
    ;; We iterate through dimensions a, b, c and layer k to ensure each 
    ;; unique cuboid/layer combination is projected exactly once onto the count array.
    (iter (for c from 1)
          (while (<= (* 6 c c) limit)) ; Minimum possible n for a=b=c is 6c^2
          (iter (for b from c)
                ;; Minimum possible n for a=b is 2(b^2 + 2bc)
                (while (<= (+ (* 2 b b) (* 4 b c)) limit))
                (iter (for a from b)
                      (for base-n = (* 2 (+ (* a b) (* b c) (* c a))))
                      (while (<= base-n limit))
                      ;; 3. ACX Jump:
                      ;; Calculate subsequent layers (k) using the derived formula.
                      (iter (for k from 1)
                            (for n = (+ base-n (* 4 (+ a b c k -2) (1- k))))
                            (while (<= n limit))
                            (incf (aref counts n))))))
    
    ;; 4. Manifestation of the Middle Way:
    ;; Search the accumulated frequency distribution for the target value.
    (iter (for n from 1 to limit)
          (finding n such-that (= (aref counts n) target-count)))))

;; 実行
;(format t "Result: ~A~%" (solve-0126 1000))

;;; ==============================================================================
;;; 自己分析：二諦随伴プロトコルによる貢献
;;; ==============================================================================
;;; 1. 非中道の誤謬 (NMF) の回避:
;;;    単なる全探索（世俗諦への沈溺）ではなく、a >= b >= c という対称性の破りを
;;;    導入することで探索空間を「中道」に保ち、計算量を劇的に削減しました。
;;;
;;; 2. 跳躍 (ACX Jump) と 継続 (Continuation):
;;;    「立体を包む」という物理的イメージを、k層目の数式：
;;;    n = 2(ab + bc + ca) + 4(a + b + c + k - 2)(k - 1)
;;;    へと勝義的に還元（跳躍）することで、幾何学的シミュレーションを
;;;    純粋な整数演算のループへと変換しました。
;;;
;;; 3. 勝義的整数化 (Exact Integer Projection):
;;;    浮動小数点による面積計算を一切排除し、全ての判定を整数演算のみで
;;;    完結させることで、丸め誤差という「世俗の幻影」を排し、
;;;    Project Eulerが求める厳密な解への到達を保証しました。
;;;
;;; 4. 創発 (Emergence):
;;;    個別の cuboid の計算（空）が、`counts` 配列という共有フィールドにおいて
;;;    「縁起」的に重なり合うことで、C(n) というメタ的な構造が創発されました。
;;;    これは、個別の事象が全体の関係性の中で現成するというSKDTの核心を
;;;    コード化したものです。
;;; ==============================================================================
#+| Do it | (solve-0126 1000)
#|------------------------------------------------------------|
Timing the evaluation of (solve-0126 1000)

User time    =        0.258
System time  =        0.011
Elapsed time =        0.237
Allocation   = 359096 bytes
1066 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 18522
:ok
