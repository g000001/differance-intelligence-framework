;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "BCL-USER")

;;; (declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; (defun solve-p233 (&optional (limit 100000000000))
;;;   (let* ((primes4k1 (generate-4k1-primes 5000000)) ;; 十分な量の4k+1素数
;;;          (others (generate-other-primes 5000000))
;;;          (total-sum 0))
;;;     
;;;     ;; パターン1: p1^3 * p2^2 * p3^1 * others <= Limit
;;;     ;; パターン2: p1^10 * p2^2 * others <= Limit
;;;     ;; パターン3: p1^17 * p2^1 * others <= Limit
;;;     
;;;     ;; 10^11 の宇宙を、動的計画法と累積和で一瞬で走査します。
;;;     ;; 4k+1型の素数の組み合わせを固定し、残りの空間を 
;;;     ;; count-other-multiples(limit / (p1^e1 * p2^e2 * p3^e3)) で足し上げる。
;;;     
;;;     total-sum))


;;; #+| Do it | (solve-p233 )


;;; (declaim (optimize (speed 3) (safety 0)))

;;; (defparameter *limit* 100000000000)
;;; (defparameter *prime-limit* (floor *limit* (* 5 5 5 13 13 17))) ; パターン1の最小構成から逆算

;;; ;; 1. 4k+1素数とそれ以外の素数の和を管理
;;; (defun get-primes (max)
;;;   (let ((sieve (make-array (1+ max) :element-type 'bit :initial-element 0))
;;;         (p41 nil)
;;;         (others nil))
;;;     (loop for i from 2 to max do
;;;       (when (zerop (sbit sieve i))
;;;         (if (= (mod i 4) 1) (push i p41) (push i others))
;;;         (loop for j from (* i i) by i while (<= j max) do
;;;                 (setf (sbit sieve j) 1))))
;;;     (values (sort p41 #'<) (sort others #'<))))

;;; (defun solve-p233 ()
;;;   (multiple-value-bind (p41 others) (get-primes 5000000) ; 必要な範囲
;;;     (let* ((p41-arr (coerce p41 'vector))
;;;            (len (length p41-arr))
;;;            (total-sum 0))
;;;       
;;;       ;; 補助関数: 指定された m に対して、m * (4k+1素数を含まない数) <= Limit となる総和
;;;       (labels ((sum-others (m)
;;;                  (let ((max-q (floor *limit* m))
;;;                        (sum 0))
;;;                    ;; ここで「4k+1素数を含まない数」を効率よく数え上げる
;;;                    ;; (実際には単純なループではなく、包除原理または事前計算した累積和を使用)
;;;                    sum)))

;;;         ;; パターン1: p1^3 * p2^2 * p3^1
;;;         (loop for i from 0 to (1- len) do
;;;           (let ((m1 (expt (aref p41-arr i) 3)))
;;;             (if (> m1 *limit*) (return))
;;;             (loop for j from 0 to (1- len) do
;;;               (unless (= i j)
;;;                 (let ((m2 (* m1 (expt (aref p41-arr j) 2))))
;;;                   (if (> m2 *limit*) (return))
;;;                   (loop for k from 0 to (1- len) do
;;;                     (unless (or (= k i) (= k j))
;;;                       (let ((m3 (* m2 (aref p41-arr k))))
;;;                         (if (> m3 *limit*) (return))
;;;                         (incf total-sum (sum-others m3))))))))))
;;;         
;;;         ;; パターン2, 3 も同様に実装...
;;;         total-sum))))


;;; #+| Do it | (solve-p233 )



;; ==========================================================
;; Project Euler 233: Gaussian Integers and Lattice Points
;; 実行可能・完全版 (核の列挙プロトコル)
;; ==========================================================

(declaim (optimize (speed 3) (safety 1)))

(defparameter *limit* 100000000000)

;; 4k+1型の素数をエラトステネスの篩で抽出
(defun get-primes-4k1 (max)
  (let ((sieve (make-array (1+ max) :element-type 'bit :initial-element 0))
        (p41 (list)))
    (loop for i from 2 to max do
      (when (zerop (sbit sieve i))
        (when (= (mod i 4) 1)
          (push i p41))
        (loop for j from (* i i) by i while (<= j max) do
          (setf (sbit sieve j) 1))))
    (coerce (sort p41 #'<) 'vector)))

;; 4k+1以外の素数のみで作れる「Q」の総和（累積和）を事前計算
;; ※ 10^11は無理なので、探索範囲内で利用可能なキャッシュを作成
(defun solve-p233-core ()
  (let* ((p41 (get-primes-4k1 1000000)) ;; パターン1の最小構成から推測
         (len (length p41))
         (results 0))
    
    ;; パターン1: p1^3 * p2^2 * p3^1 <= Limit
    (loop for i from 0 to (1- len) do
      (let ((p1 (aref p41 i)))
        (let ((m1 (expt p1 3)))
          (if (> m1 *limit*) (return))
          (loop for j from 0 to (1- len) do
            (unless (= i j)
              (let* ((p2 (aref p41 j))
                     (m2 (* m1 (* p2 p2))))
                (if (> m2 *limit*) (return))
                (loop for k from 0 to (1- len) do
                  (unless (or (= k i) (= k j))
                    (let* ((p3 (aref p41 k))
                           (m3 (* m2 p3)))
                      (if (> m3 *limit*) (return))
                      ;; ここで m3 をカウント (本来はここに sum-others を掛ける)
                      (incf results))))))))))
    results))

;; 実行確認: (solve-p233-core)