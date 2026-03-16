;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0248 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0248)

#||
(cl-text "Project Euler 248 Logic Projection - Refined"
  (cl-comment "1. Inverse Totient Generation remains valid. Candidates are bounded.")
  (cl-comment "2. Bijective DFS with Boundary Correction: When remaining phi reaches 1, we must mathematically account for phi(2) = 1, which generates a symmetric even solution for every valid odd solution.")
  (forall (n)
    (if (and (= (phi n) 13!) (oddp n))
        (= (phi (* n 2)) 13!)))
  
  (cl-comment "3. Complete Evaluation ensures no valid branch is prematurely pruned.")
)
||#


(defun mod-exp (base exp mod)
  (let ((res 1)
        (b (mod base mod)))
    (iterate
      (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (* res b) mod)))
      (setf b (mod (* b b) mod)
            exp (ash exp -1)))
    res))

(defun is-prime (n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= n 3) t)
        ((evenp n) nil)
        (t (let ((d (1- n))
                 (s 0))
             (iterate
               (while (evenp d))
               (setf d (ash d -1))
               (incf s))
             (iterate
               (for a in-vector #(2 3 5 7 11 13 17 19 23))
               (when (>= a n) (leave t))
               (let ((x (mod-exp a d n)))
                 (unless (or (= x 1) (= x (1- n)))
                   (let ((prime-p nil))
                     (iterate
                       (for r from 1 below s)
                       (setf x (mod (* x x) n))
                       (when (= x (1- n))
                         (setf prime-p t)
                         (leave)))
                     (unless prime-p (return-from is-prime nil))))))
             t))))

(defun get-divisors (prime-factors)
  (let ((divs (list 1)))
    (iterate
      (for (p . max-k) in prime-factors)
      (let ((new-divs nil))
        (iterate
          (for d in divs)
          (let ((mult 1))
            (iterate
              (for k from 0 to max-k)
              (push (* d mult) new-divs)
              (setf mult (* mult p)))))
        (setf divs new-divs)))
    (sort divs #'<)))

(defun solve ()
  (let* ((m 6227020800) ;; 13! 
         (prime-factors '((2 . 10) (3 . 5) (5 . 2) (7 . 1) (11 . 1) (13 . 1)))
         (divisors (get-divisors prime-factors))
         (candidates nil)
         ;; 解の数は前回(約13万)から倍増すると予測されるためキャパシティを拡大
         (results (make-array 300000 :fill-pointer 0 :adjustable t)))
    
    ;; 1. 約数から n を構成し得る素数の候補を生成
    (iterate
      (for d in divisors)
      (for p = (1+ d))
      (when (is-prime p)
        (let ((powers nil)
              (phi d)
              (val p))
          (iterate
            (while (= (mod m phi) 0))
            (push (cons phi val) powers)
            (if (= (mod m p) 0)
                (setf phi (* phi p)
                      val (* val p))
                (leave)))
          (push (nreverse powers) candidates))))
          
    ;; 2. DFSの効率化のため、素数の値で降順ソート。これにより p=2 は必ずリストの最後になる。
    (setf candidates (sort candidates #'> :key (lambda (c) (cdar c))))
    
    ;; 3. 修正されたDFS（深さ優先探索）
    (labels ((dfs (cands current-phi current-n)
               (cond
                 ;; 目標の phi に到達した場合
                 ((= current-phi 1)
                  (vector-push-extend current-n results)
                  ;; p=2 (phi=1) が未処理として残っている場合は、対称解(* 2)も回収する
                  (iterate
                    (for c in cands)
                    (when (= (caar c) 1) ;; caarが1になるのは p=2 の k=1 のみ
                      (vector-push-extend (* current-n (cdar c)) results))))
                 ;; まだ候補が残っている場合
                 (cands
                  ;; 選択肢1: この素数を使わない（スキップ）
                  (dfs (cdr cands) current-phi current-n)
                  ;; 選択肢2: この素数の有効な冪乗のいずれかを使う
                  (iterate
                    (for (phi-k . val-k) in (car cands))
                    (when (= (mod current-phi phi-k) 0)
                      (dfs (cdr cands) (truncate current-phi phi-k) (* current-n val-k))))))))
      
      (format t "Precomputed ~A valid prime candidates.~%" (length candidates))
      (dfs candidates m 1)
      (format t "Found ~A combinations.~%" (length results))
      
      ;; 4. 結果をソートして 150,000 番目 (インデックス 149,999) を取得
      (let ((sorted-results (sort results #'<)))
        (aref sorted-results 149999)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Precomputed 459 valid prime candidates.
Found 182752 combinations.

User time    =        0.607
System time  =        0.026
Elapsed time =        0.501
Allocation   = 4478560 bytes
4020 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 23507044290
:ok