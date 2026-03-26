;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0781 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0781)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
【自己批判とオフバイワン・バグの撲滅】
前回の配列外参照（subscript exceeds limit）は、seriesのせいではなく、
私が「k用とm用で配列サイズをケチって別々にギリギリのサイズで確保する」という
典型的な悪癖（Geminiの定番バグ）を発動させたためです。
ここでは、すべての作業配列を統一して十分なサイズ `(+ n 5)` で確保します。
数百KBのメモリ余裕を持たせるだけで、この種の境界バグは完全に消滅し、
本質的な母関数 G(h) = B(h)/A(h) の畳み込みアルゴリズムだけが安全に実行されます。
||#

(defconstant +modulo+ 1000000007)

(defun make-uint64-array (size)
  (make-array size :element-type '(unsigned-byte 64) :initial-element 0))

(defun power-mod (base exp)
  "base^exp mod +modulo+ を計算する"
  (let ((res 1)
        (b (mod base +modulo+)))
    (do () ((<= exp 0) res)
      (when (oddp exp)
        (setf res (mod (* res b) +modulo+)))
      (setf b (mod (* b b) +modulo+))
      (setf exp (ash exp -1)))))

(defun mod-inverse (n)
  "フェルマーの小定理によるモジュラ逆数"
  (power-mod n (- +modulo+ 2)))

(defun solve (&optional (n 50000))
  (assert (evenp n))
  (let* ((m (ash n -1))
         (k n)
         (safe-size (+ n 5))
         (fact (make-uint64-array safe-size))
         (inv-fact (make-uint64-array safe-size))
         (d-arr (make-uint64-array safe-size))
         (e-arr (make-uint64-array safe-size))
         (a-arr (make-uint64-array safe-size))
         (b-arr (make-uint64-array safe-size))
         (i-arr (make-uint64-array safe-size)))
    
    ;; 1. 階乗と逆元の事前計算
    (setf (aref fact 0) 1)
    (iterate ((i (scan-range :from 1 :upto k)))
      (setf (aref fact i) (mod (* (aref fact (1- i)) i) +modulo+)))
    
    (setf (aref inv-fact k) (mod-inverse (aref fact k)))
    (iterate ((i (scan-range :from (1- k) :downto 0 :by -1)))
      (setf (aref inv-fact i) (mod (* (aref inv-fact (1+ i)) (1+ i)) +modulo+)))
      
    ;; 2. 完全順列 (Derangement) の数 D_k の計算
    (setf (aref d-arr 0) 1)
    (setf (aref d-arr 1) 0)
    (iterate ((i (scan-range :from 2 :upto k)))
      (let ((term (if (evenp i) 1 (- +modulo+ 1))))
        (setf (aref d-arr i) (mod (+ (* i (aref d-arr (1- i))) term) +modulo+))))
        
    ;; 3. E_k = sum_{j=0}^{k} D_j / j! の計算
    (setf (aref e-arr 0) 1)
    (iterate ((i (scan-range :from 1 :upto k)))
      (let ((term (mod (* (aref d-arr i) (aref inv-fact i)) +modulo+)))
        (setf (aref e-arr i) (mod (+ (aref e-arr (1- i)) term) +modulo+))))
        
    ;; 4. 母関数の係数 A_m, B_m の構築
    (let ((inv-2 (mod-inverse 2))
          (inv-pow2 1))
      (iterate ((i (scan-range :from 0 :upto m)))
        (let ((inv-pow2-mfact (mod (* inv-pow2 (aref inv-fact i)) +modulo+)))
          (setf (aref a-arr i) (mod (* (aref d-arr (* 2 i)) inv-pow2-mfact) +modulo+))
          (let ((tmp (mod (* (aref fact (* 2 i)) inv-pow2-mfact) +modulo+)))
            (setf (aref b-arr i) (mod (* tmp (aref e-arr (* 2 i))) +modulo+))))
        (setf inv-pow2 (mod (* inv-pow2 inv-2) +modulo+))))
        
    ;; 5. 多項式の逆元 I(h) = 1/A(h) の計算
    (setf (aref i-arr 0) (mod-inverse (aref a-arr 0)))
    (iterate ((i (scan-range :from 1 :upto m)))
      (let ((sum 0))
        (iterate ((j (scan-range :from 1 :upto i)))
          (incf sum (* (aref a-arr j) (aref i-arr (- i j))))
          (when (= (logand j 7) 0)
            (setf sum (mod sum +modulo+))))
        (setf sum (mod sum +modulo+))
        (setf (aref i-arr i) (mod (- +modulo+ sum) +modulo+))
        (when (= (mod i 5000) 0)
          (format t "観測: I(h)の係数 ~D/~D まで計算完了~%" i m))))
          
    ;; 6. F(2M) = [h^M] (B(h) * I(h)) の抽出
    (let ((ans 0))
      (iterate ((j (scan-range :from 0 :upto m)))
        (incf ans (* (aref b-arr j) (aref i-arr (- m j))))
        (when (= (logand j 7) 0)
          (setf ans (mod ans +modulo+))))
      (setf ans (mod ans +modulo+))
      (format t "F(~D) = ~D~%" n ans)
      ans)))

#+| Do it | (project-euler-0781:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
観測: I(h)の係数 5000/25000 まで計算完了
観測: I(h)の係数 10000/25000 まで計算完了
観測: I(h)の係数 15000/25000 まで計算完了
観測: I(h)の係数 20000/25000 まで計算完了
観測: I(h)の係数 25000/25000 まで計算完了
F(50000) = 162450870

User time    =       11.578
System time  =        0.098
Elapsed time =       11.554
Allocation   = 2217105704 bytes
5797 Page faults
GC time      =        0.030
 |------------------------------------------------------------|#
;;→ 162450870
:ok