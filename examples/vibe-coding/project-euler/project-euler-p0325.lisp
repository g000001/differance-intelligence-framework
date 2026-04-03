;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0325 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0325)

(defun get-fibs (n)
  "do マクロの並列更新を用いて、正確に F_n と F_{n+1} を生成する"
  (do ((i 0 (1+ i))
       (a 0 b)          ;; a は古い b を受け取る
       (b 1 (+ a b)))   ;; b は「更新前の古い a」と b の和を受け取る (並列評価)
      ((>= i n) (values a b))))

(defun compute-S (N)
  "O(log N) で S(N) を再帰的に計算する"
  ;; 無理数 alpha = (sqrt(5)-1)/2 を有理数 F_200 / F_201 で近似する。
  (multiple-value-bind (fa fb) (get-fibs 200)
    (labels ((recur (n)
               (if (= n 0)
                   (values 0 0 0)
                   (let* ((m (truncate (* fa n) fb)))
                     (multiple-value-bind (s0 s1 s2) (recur m)
                       ;; 幾何学的反転公式による O(1) での漸化式ステップ
                       (let* ((new-s0 (- (* n m) 
                                         (truncate (* m (1+ m)) 2) 
                                         s0))
                              (new-s2 (+ (- (* n (* m m))
                                            (truncate (* m (1+ m) (1- (* 4 m))) 6))
                                         s0
                                         (- (* 2 s1))))
                              (new-s1 (truncate (- (* n (1+ n) m)
                                                   (truncate (* m (1+ m) (+ m 2)) 3)
                                                   s0
                                                   (* 2 s1)
                                                   s2)
                                                2)))
                         (values new-s0 new-s1 new-s2)))))))
      (multiple-value-bind (s0 s1 s2) (recur N)
        (let ((term1 (truncate (* N (1+ N) (1- N)) 2)))
          ;; S_0 と S_2 は x^2 ≡ x (mod 2) の性質から和が必ず偶数となる
          (- term1 s1 (truncate (+ s0 s2) 2)))))))

(defun solve ()
  (format t "観測: テストケース S(10) を検証中...~%")
  (let ((ans10 (compute-S 10)))
    (format t "観測: S(10) = ~D (Expected: 211)~%" ans10))
  
  (format t "観測: テストケース S(10^4) を検証中...~%")
  (let ((ans10k (compute-S 10000)))
    (format t "観測: S(10^4) = ~D (Expected: 230312207313)~%" ans10k))
  
  (format t "観測: 本探索 S(10^16) を実行中...~%")
  (let* ((ans (compute-S 10000000000000000))
         (modulo (expt 7 10))
         (mod-ans (mod ans modulo)))
    (format t "観測: S(10^16) = ~D~%" ans)
    (format t "Answer: ~D~%" mod-ans)
    mod-ans))

#+| Do it | (project-euler-0325:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: テストケース S(10) を検証中...
観測: S(10) = 211 (Expected: 211)
観測: テストケース S(10^4) を検証中...
観測: S(10^4) = 230312207313 (Expected: 230312207313)
観測: 本探索 S(10^16) を実行中...
観測: S(10^16) = 230327668541684176515052475525037682834286696168
Answer: 54672965

User time    =        0.000
System time  =        0.000
Elapsed time =        0.001
Allocation   = 43712 bytes
2 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 54672965
:ok
