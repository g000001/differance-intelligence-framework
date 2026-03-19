;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0987 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0987)

;; 階乗の事前計算テーブル (0! から 9!)
(defvar *fact* (make-array 10 :initial-contents '(1 1 2 6 24 120 720 5040 40320 362880) :element-type 'integer))

;; 10要素の配列(各要素0〜4)を30ビット整数(3ビット/要素)にパックする
(defun pack (arr)
  (let ((res 0))
    (iterate (for i from 0 to 9)
      (setf res (logior res (ash (aref arr i) (* i 3)))))
    res))

;; 30ビット整数を配列にアンパックする
(defun unpack (packed arr)
  (iterate (for i from 0 to 9)
    (setf (aref arr i) (logand (ash packed (* i -3)) 7))))

;; 各ランク(A~K)の使用枚数を計算する
(defun compute-k-arr (arr)
  (let ((k (make-array 13 :element-type 'fixnum)))
    (setf (aref k 0) (+ (aref arr 0) (aref arr 9))) ; Ace (High and Low)
    (setf (aref k 1) (+ (aref arr 0) (aref arr 1))) ; 2
    (setf (aref k 2) (+ (aref arr 0) (aref arr 1) (aref arr 2))) ; 3
    (setf (aref k 3) (+ (aref arr 0) (aref arr 1) (aref arr 2) (aref arr 3))) ; 4
    (setf (aref k 4) (+ (aref arr 0) (aref arr 1) (aref arr 2) (aref arr 3) (aref arr 4))) ; 5
    (setf (aref k 5) (+ (aref arr 1) (aref arr 2) (aref arr 3) (aref arr 4) (aref arr 5))) ; 6
    (setf (aref k 6) (+ (aref arr 2) (aref arr 3) (aref arr 4) (aref arr 5) (aref arr 6))) ; 7
    (setf (aref k 7) (+ (aref arr 3) (aref arr 4) (aref arr 5) (aref arr 6) (aref arr 7))) ; 8
    (setf (aref k 8) (+ (aref arr 4) (aref arr 5) (aref arr 6) (aref arr 7) (aref arr 8))) ; 9
    (setf (aref k 9) (+ (aref arr 5) (aref arr 6) (aref arr 7) (aref arr 8) (aref arr 9))) ; 10
    (setf (aref k 10) (+ (aref arr 6) (aref arr 7) (aref arr 8) (aref arr 9))) ; J
    (setf (aref k 11) (+ (aref arr 7) (aref arr 8) (aref arr 9))) ; Q
    (setf (aref k 12) (+ (aref arr 8) (aref arr 9))) ; K
    k))

;; 有効なXのタプルを探索・生成する
(defvar *valid-x-packed* nil)

(defun generate-x (idx current-x sum-x target-sum)
  (if (= idx 10)
      (when (= sum-x target-sum)
        (let* ((x (make-array 10 :element-type 'fixnum)))
          (iterate (for i from 0 to 9)
                   (for el in (reverse current-x))
                   (setf (aref x i) el))
          (let ((k-arr (compute-k-arr x))
                (valid t))
            (iterate (for i from 0 to 12)
              (when (> (aref k-arr i) 4)
                (setf valid nil)))
            (when valid
              (push (pack x) *valid-x-packed*)))))
      (iterate (for val from 0 to (min 4 (- target-sum sum-x)))
        (generate-x (1+ idx) (cons val current-x) (+ sum-x val) target-sum))))

;; 事前計算のための構造体
(defstruct x-info
  (packed 0 :type fixnum)
  arr
  k-arr
  (prod-4-minus-k 1 :type integer))

(defvar *x-infos* nil)

(defun prepare-x-infos ()
  (setf *x-infos* nil)
  (iterate (for px in *valid-x-packed*)
    (let* ((arr (make-array 10 :element-type 'fixnum))
           (_ (unpack px arr))
           (k-arr (compute-k-arr arr))
           (p4mk 1))
      (declare (ignore _))
      (iterate (for i from 0 to 12)
        (setf p4mk (* p4mk (aref *fact* (- 4 (aref k-arr i))))))
      (push (make-x-info :packed px :arr arr :k-arr k-arr :prod-4-minus-k p4mk)
            *x-infos*))))

;; y <= x を高速に判定する
(defun y-le-x-p (packed-y packed-x)
  (let ((y packed-y) (x packed-x))
    (iterate (for i from 0 to 9)
      (if (> (logand y 7) (logand x 7))
          (return-from y-le-x-p nil))
      (setf y (ash y -3))
      (setf x (ash x -3)))
    t))

;; 階乗の差積の計算 Π (x_i - y_i)!
(defun get-prod-fact-diff (x-arr y-arr)
  (let ((res 1))
    (iterate (for i from 0 to 9)
      (setf res (* res (aref *fact* (- (aref x-arr i) (aref y-arr i))))))
    res))

;; 階乗の差積の計算 Π (4 - k_r)!
(defun get-prod-4-minus-k (k-arr)
  (let ((res 1))
    (iterate (for i from 0 to 12)
      (setf res (* res (aref *fact* (- 4 (aref k-arr i))))))
    res))

;; オーバーラップグラフの独立集合の生成 (25個)
(defvar *ind-sets* nil)
(defun generate-ind-sets ()
  (setf *ind-sets* (list 0))
  (iterate (for i from 1 to 10)
    (push (ash 1 (* (1- i) 3)) *ind-sets*))
  (let ((pairs '((1 6) (1 7) (1 8) (1 9)
                 (2 7) (2 8) (2 9) (2 10)
                 (3 8) (3 9) (3 10)
                 (4 9) (4 10)
                 (5 10))))
    (iterate (for p in pairs)
      (let ((i (first p))
            (j (second p)))
        (push (+ (ash 1 (* (1- i) 3)) (ash 1 (* (1- j) 3))) *ind-sets*)))))

;; 4つのスート(独立集合)への彩色からYベクトルの重み(係数)を計算する
(defvar *n-y* (make-hash-table :test 'eql))
(defun generate-n-y ()
  (clrhash *n-y*)
  (iterate (for s1 in *ind-sets*)
    (iterate (for s2 in *ind-sets*)
      (iterate (for s3 in *ind-sets*)
        (iterate (for s4 in *ind-sets*)
          (let ((y (+ s1 s2 s3 s4)))
            (incf (gethash y *n-y* 0))))))))

(defun solve (&optional (target-sum 8))
  (format t "Initializing configurations for ~D disjoint straights...~%" target-sum)
  (setf *valid-x-packed* nil)
  (generate-x 0 nil 0 target-sum)
  (format t "Generated ~D valid tuples for X.~%" (length *valid-x-packed*))
  
  (prepare-x-infos)
  (generate-ind-sets)
  (generate-n-y)
  (format t "Generated ~D distinct states for Y from Overlap Graph.~%" (hash-table-count *n-y*))
  
  (let ((ans 0))
    (maphash (lambda (py n-y)
               (let* ((y-arr (make-array 10 :element-type 'fixnum))
                      (_ (unpack py y-arr))
                      (y-prime (compute-k-arr y-arr))
                      (y-sum 0))
                 (declare (ignore _))
                 (iterate (for i from 0 to 9) (incf y-sum (aref y-arr i)))
                 
                 (let ((term1 (* n-y (get-prod-4-minus-k y-prime))))
                   ;; 包除原理による正負の反転 (-1)^|Y|
                   (when (oddp y-sum)
                     (setf term1 (- term1)))
                   
                   (let ((sum-x 0))
                     (iterate (for xi in *x-infos*)
                       (when (y-le-x-p py (x-info-packed xi))
                         (let ((pfd (get-prod-fact-diff (x-info-arr xi) y-arr))
                               (p4mk (x-info-prod-4-minus-k xi)))
                           ;; Common Lisp の正確な有理数演算を活用し誤差を完全に排除
                           (incf sum-x (/ 1 (* pfd p4mk))))))
                     
                     (incf ans (* term1 sum-x))))))
             *n-y*)
    (format t "Final ans = ~D~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Initializing configurations for 8 disjoint straights...
Generated 1599 valid tuples for X.
Generated 8870 distinct states for Y from Overlap Graph.
Final ans = 11044580082199135512

User time    =        0.883
System time  =        0.019
Elapsed time =        0.837
Allocation   = 20654344 bytes
607 Page faults
GC time      =        0.002
 |------------------------------------------------------------|#
;;→ 11044580082199135512
:ok