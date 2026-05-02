;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0117 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0117)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)

#||
(cl-comment "=== 6. Exact Integer Projection (勝義的整数化による浮動小数点の排除) ===")
  (cl-comment "本問題は組合せ論的な数え上げであり、動的計画法（DP）を用いることで、浮動小数点演算を一切介さず、純粋な整数加算のみで完結する。")
  (forall (a p)
    (if (and (OptimizedAlgorithm a)
             (solves a p)
             (involves_irrational_numbers p))
        (and (eliminates_floating_point a)
             (uses_exact_integer_arithmetic a)
             (avoids_truncation_errors a))))

  (cl-comment "=== 7. Bijective Generation (対称性の破れと一意生成の厳密化) ===")
  (cl-comment "タイル配置の順序が区別されるため、末尾に置くタイルの長さ（1, 2, 3, 4）によって状態を遷移させることで、全単射な数え上げを実現する。")
  (forall (a)
    (if (uses_parameterized_generation a)
        (and (eliminates_symmetric_duplications a)
             (defines_strict_fundamental_domain a)
             (guarantees_bijective_counting a))))

  (cl-comment "=== 8. Verification against Emptiness (境界値における自己検算) ===")
  (cl-comment "問題文の例 T(5)=15 を用いて、漸化式 f(n) = f(n-1) + f(n-2) + f(n-3) + f(n-4) の妥当性を確認した。")
  (cl-comment "f(0)=1, f(1)=1, f(2)=2, f(3)=4, f(4)=8 => f(5)=8+4+2+1=15。論理的整合性を確認。")
  (forall (a p)
    (if (and (ACX_Jump j) (target_of j a) (has_example_cases p))
        (and (performs_mental_trace a)
             (matches_example_cases a)
             (prevents_premature_manifestation a))))

(cl-comment "=== 9. Axiomatic Grounding (公理的定礎と幻覚の超克) ===")
  (cl-comment "本問題は線形漸化式に還元される。N=50 という制約は O(N) で十分に高速であり、行列累乗を用いた O(log N) への最適化も可能だが、今回はシンプルかつ堅牢な DP を採用する。")
  (forall (p a)
    (if (and (Problem p) (requires_ACX_jump a))
        (and (fully_consumes_exact_rules p)
             (avoids_inductive_guessing a)
             (grounds_in_deductive_logic a))))



||#

(defun solve (&optional (target-n 50))
  "Project Euler P117: カウント・タイル・コンビネーション
長さ target-n の列を、長さ1, 2, 3, 4のタイルで埋める方法の総数を求める。
漸化式 f(n) = f(n-1) + f(n-2) + f(n-3) + f(n-4) を使用する。"
  (let ((dp (make-array (1+ target-n) :initial-element 0)))
    ;; 基底状態: 長さ0を埋める方法は「何もしない」の1通り
    (setf (aref dp 0) 1)
    
    (format t "Calculating tiling ways for N=1 to ~A...~%" target-n)
    
    (iterate (for n from 1 to target-n)
             ;; 各ステップで、最後に置くタイルの長さを 1, 2, 3, 4 と仮定して加算
             (setf (aref dp n)
                   (+ (if (>= n 1) (aref dp (- n 1)) 0)
                      (if (>= n 2) (aref dp (- n 2)) 0)
                      (if (>= n 3) (aref dp (- n 3)) 0)
                      (if (>= n 4) (aref dp (- n 4)) 0)))
             
             ;; 中間ログ: 小さな値での検証用
             (when (<= n 5)
               (format t "f(~A) = ~A~%" n (aref dp n))))
    
    (let ((result (aref dp target-n)))
      (format t "~%Final result for N=~A: ~A~%" target-n result)
      result)))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating tiling ways for N=1 to 50...
f(1) = 1
f(2) = 2
f(3) = 4
f(4) = 8
f(5) = 15

Final result for N=50: 100808458960497

User time    =        0.000
System time  =        0.000
Elapsed time =        0.016
Allocation   = 1680 bytes
13 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 100808458960497
:ok
