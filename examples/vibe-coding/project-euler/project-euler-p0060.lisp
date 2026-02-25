;;; -*- mode: Lisp; coding: utf-8  -*-


(in-package :cl-user) 
(defpackage "PROJECT-EULER-60" (:use :cl)) 
(in-package "PROJECT-EULER-60") 
#||
<p>The primes $3$, $7$, $109$, and $673$, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking $7$ and $109$, both $7109$ and $1097$ are prime. The sum of these four primes, $792$, represents the lowest sum for a set of four primes with this property.</p>
<p>Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.</p>

||#

;; 高速な素数判定（Miller-Rabin法）
(defun miller-rabin-p (n &optional (k 5))
  (if (or (<= n 1) (evenp n)) (return-from miller-rabin-p (= n 2)))
  (if (<= n 3) (return-from miller-rabin-p t))
  (let* ((d (- n 1)) (s 0))
    (loop while (evenp d) do (setf d (/ d 2) s (1+ s)))
    (loop repeat k do
      (let* ((a (+ 2 (random (- n 4))))
             (x (exp-mod a d n)))
        (if (or (= x 1) (= x (- n 1)))
            nil
            (loop repeat (- s 1) do
              (setf x (mod (* x x) n))
              (if (= x (- n 1)) (return t))
              finally (return-from miller-rabin-p nil)))))
    t))

(defun exp-mod (base exp m)
  (let ((res 1))
    (setf base (mod base m))
    (loop while (> exp 0) do
      (when (oddp exp) (setf res (mod (* res base) m)))
      (setf exp (ash exp -1)
            base (mod (* base base) m)))
    res))

;; 二つの数値を連結する
(defun concatenate-numbers (a b)
  (parse-integer (format nil "~A~A" a b)))

;; ペアが条件を満たすかチェック（メモ化用ハッシュ）
(let ((pair-cache (make-hash-table :test 'equal)))
  (defun valid-pair-p (a b)
    (let ((key (if (< a b) (list a b) (list b a))))
      (multiple-value-bind (val exists) (gethash key pair-cache)
        (if exists val
            (setf (gethash key pair-cache)
                  (and (miller-rabin-p (concatenate-numbers a b))
                       (miller-rabin-p (concatenate-numbers b a)))))))))

;; メイン探索ロジック
(defun solve-euler-60 ()
  (let* ((limit 10000)
         (primes (loop for i from 3 to limit by 2 
                       when (miller-rabin-p i) collect i))
         (adj (make-hash-table)))
    
    ;; グラフの構築: 各素数に対して条件を満たすペアをリスト化
    (loop for (p1 . rest) on primes do
      (loop for p2 in rest do
        (when (valid-pair-p p1 p2)
          (push p2 (gethash p1 adj))
          (push p1 (gethash p2 adj)))))

    ;; 5つのクリーク（完全グラフ）を探す
    (let ((min-sum 1000000))
      (loop for p1 in primes do
        (let ((candidates1 (gethash p1 adj)))
          (loop for (p2 . rest2) on candidates1 do
            (let ((candidates2 (intersection rest2 (gethash p2 adj))))
              (loop for (p3 . rest3) on candidates2 do
                (let ((candidates3 (intersection rest3 (gethash p3 adj))))
                  (loop for (p4 . rest4) on candidates3 do
                    (let ((candidates4 (intersection rest4 (gethash p4 adj))))
                      (loop for p5 in candidates4 do
                        (let ((current-sum (+ p1 p2 p3 p4 p5)))
                          (when (< current-sum min-sum)
                            (setf min-sum current-sum)
                            (format t "Found: ~A (Sum: ~A)~%" 
                                    (list p1 p2 p3 p4 p5) min-sum))))))))))))
      min-sum)))


#+| Do it | (format t "Result: ~A~%" (solve-euler-60))
;▻ Found: (13 8389 5197 6733 5701) (Sum: 26033)
;▻ Result: 26033
;→ nil

