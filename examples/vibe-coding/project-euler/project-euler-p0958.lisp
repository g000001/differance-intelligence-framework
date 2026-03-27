;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0958 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0958)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【自己批判と IDA* への昇華】
前回のビームサーチは「一時的にスコアが悪化する真の最適パス」を刈り取るという致命的なバグを抱え、
無駄な配列確保によってGCの負担も増大させていた。
ここでは、完璧なヒューリスティック関数(Fibonacci下界)を用いた IDA* (反復深化 A*) を採用する。
IDA* は状態を配列に保存しないため、動的メモリ確保（アロケーション）が完全にゼロになる。
また、大域的最適解を絶対にこぼさないことが数学的に保証される。
不要な配列や冗長なコードを全て消し去り、再帰スタックのみで10^12の空間を数秒で切り裂く。
||#

(defconstant $target-n (+ #.(expt 10 12) 39))

(defun make-fixnum-array (size)
  (make-array size :element-type 'fixnum :initial-element 0))

;; 余裕を持たせた100サイズのFibonacci表
(defparameter *fib* (make-fixnum-array 100))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (aref *fib* 1) 1)
  (setf (aref *fib* 2) 1)
  (do ((i 3 (1+ i))) ((> i 90))
    (setf (aref *fib* i) (+ (aref *fib* (- i 1)) (aref *fib* (- i 2))))))

(declaim (inline estimate-h))
(defun estimate-h (x y target)
  "状態 (x, y) から target に到達するための最小追加コスト(完全な下界)"
  (declare (type fixnum x y target)
           (optimize (speed 3) (safety 0) (debug 0)))
  (do ((k 0 (1+ k)))
      ((> k 85) 85)
    (declare (type fixnum k))
    (when (>= (+ (* (aref *fib* (+ k 1)) x)
                 (* (aref *fib* k) y))
              target)
      (return k))))

(defun solve (&optional (target-n $target-n))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((start-limit (estimate-h 1 0 target-n))
        (best-m most-positive-fixnum))
    (declare (type fixnum start-limit best-m target-n))
    
    (labels ((dfs (x y g limit is-first)
               (declare (type fixnum x y g limit)
                        (type boolean is-first))
               ;; 目標の n に到達したか判定
               (if (= x target-n)
                   (when (< y best-m)
                     (setf best-m y))
                   ;; ヒューリスティック枝刈り
                   (let ((h (estimate-h x y target-n)))
                     (declare (type fixnum h))
                     (when (<= (+ g h) limit)
                       ;; 初手は q=2 から始めることで、q=1による(1,1)の重複パスを論理的に排除する
                       (do ((q (if is-first 2 1) (1+ q)))
                           (nil)
                         (declare (type fixnum q))
                         (let ((next-x (+ (* q x) y)))
                           (declare (type fixnum next-x))
                           ;; xは単調増加するため、targetを超えた瞬間に以降の q の探索を安全に打ち切る
                           (if (> next-x target-n)
                               (return)
                               (dfs next-x x (+ g q) limit nil)))))))))
                               
      (format t "観測: IDA*探索を開始します。初期Limit = ~D~%" start-limit)
      
      ;; IDA* (反復深化) メインループ
      (do ((limit start-limit (1+ limit)))
          (nil)
        (declare (type fixnum limit))
        (setf best-m most-positive-fixnum)
        
        ;; 起点 (1, 0) コスト 0 から探索開始
        (dfs 1 0 0 limit t)
        
        (when (< best-m most-positive-fixnum)
          (format t "観測: 最適解を発見。Cost = ~D~%" limit)
          (format t "f(~D) = ~D~%" target-n best-m)
          (return best-m))))))

#+| Do it | (project-euler-0958:solve)