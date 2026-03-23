;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0560 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0560)

#||
Project Euler 560: Coprime Nim
数論的ショートカット：
Coprime NimのGrundy数は、その数の最小の素因数が何番目の素数かによって完全に決定される。
これにより各山の初期値のGrundy数をエラトステネスの篩で O(N log log N) で算出し、
その頻度配列をFWHT (高速ウォルシュ・アダマール変換) を用いて XOR 畳み込みを行う。
Lispの 61-bit fixnum 空間での乗算は 10^18 まで Bignum への Boxing なしに計算可能であり、
ループ内の高負荷な mod 関数を算術マクロに置換することでアセンブリ級の実行速度を実現する。
||#

(defmacro add-mod (x y mod-val)
  "剰余加算：高価な mod 関数を避けるための高速マクロ"
  `(let ((sum (+ (the fixnum ,x) (the fixnum ,y))))
     (declare (type fixnum sum))
     (if (>= sum (the fixnum ,mod-val))
         (the fixnum (- sum (the fixnum ,mod-val)))
         sum)))

(defmacro sub-mod (x y mod-val)
  "剰余減算：同上"
  `(let ((diff (- (the fixnum ,x) (the fixnum ,y))))
     (declare (type fixnum diff))
     (if (< diff 0)
         (the fixnum (+ diff (the fixnum ,mod-val)))
         diff)))

(defun power-mod (base exp mod-val)
  "繰り返し二乗法による高速な剰余冪乗計算"
  (declare (type fixnum base exp mod-val))
  (let ((res 1)
        (b (mod base mod-val)))
    (declare (type fixnum res b))
    (iterate (for p initially exp then (ash p -1))
      (declare (type fixnum p))
      (while (> p 0))
      (when (oddp p)
        (setf res (mod (* res b) mod-val)))
      (setf b (mod (* b b) mod-val)))
    res))

(defun next-power-of-2 (n)
  (declare (type fixnum n))
  (let ((p 1))
    (declare (type fixnum p))
    (iterate
      (while (< p n))
      (setf p (ash p 1)))
    p))

(defun compute-frequencies (limit)
  "各Grundy値の出現頻度を、最適化されたエラトステネスの篩を用いて計算"
  (declare (type fixnum limit))
  (let* ((g (make-array limit :element-type '(unsigned-byte 32) :initial-element 0))
         (prime-idx 1))
    (declare (type (simple-array (unsigned-byte 32) (*)) g)
             (type fixnum prime-idx))
    (iterate (for i from 2 below limit)
      (declare (type fixnum i))
      (when (= (aref g i) 0) ; i は素数
        (let ((idx prime-idx))
          (declare (type fixnum idx))
          (incf prime-idx)
          (setf (aref g i) idx)
          ;; i * i が limit 未満のときのみ、倍数のマーキングループを起動する
          (let ((start (* i i)))
            (declare (type fixnum start))
            (when (< start limit)
              (iterate (for j from start below limit by i)
                (declare (type fixnum j))
                (when (= (aref g j) 0)
                  (setf (aref g j) idx))))))))
    
    (let* ((max-g (1- prime-idx))
           (m (next-power-of-2 (1+ max-g)))
           (c (make-array m :element-type 'fixnum :initial-element 0)))
      (declare (type (simple-array fixnum (*)) c)
               (type fixnum max-g m))
      (setf (aref c 0) 1) ; G(1) = 0
      (iterate (for i from 2 below limit)
        (declare (type fixnum i))
        (incf (aref c (aref g i))))
      (values c m))))

(defun fwht (a m mod-val)
  "FWHT (Fast Walsh-Hadamard Transform) の核"
  (declare (type (simple-array fixnum (*)) a)
           (type fixnum m mod-val))
  (iterate (for len initially 1 then (ash len 1))
    (declare (type fixnum len))
    (while (< len m))
    (let ((2len (ash len 1)))
      (declare (type fixnum 2len))
      (iterate (for i from 0 below m by 2len)
        (declare (type fixnum i))
        (iterate (for j from 0 below len)
          (declare (type fixnum j))
          (let* ((idx1 (+ i j))
                 (idx2 (+ idx1 len))
                 (u (aref a idx1))
                 (v (aref a idx2)))
            (declare (type fixnum idx1 idx2 u v))
            (setf (aref a idx1) (add-mod u v mod-val))
            (setf (aref a idx2) (sub-mod u v mod-val))))))))

(defun inverse-fwht (a m mod-val)
  "逆 FWHT：再度 FWHT を適用後、M のモジュラ逆数で割る"
  (declare (type (simple-array fixnum (*)) a)
           (type fixnum m mod-val))
  (fwht a m mod-val)
  (let ((inv-m (power-mod m (- mod-val 2) mod-val)))
    (declare (type fixnum inv-m))
    (iterate (for i from 0 below m)
      (declare (type fixnum i))
      (setf (aref a i) (mod (* (aref a i) inv-m) mod-val)))))

(defun solve ()
  (let* ((n 10000000)
         (k 10000000)
         (mod-val 1000000007))
    (declare (type fixnum n k mod-val))
    (format t "Computing Grundy value frequencies with N=~D...~%" n)
    (multiple-value-bind (c m) (compute-frequencies n)
      (declare (type (simple-array fixnum (*)) c)
               (type fixnum m))
      (format t "XOR domain array size M = ~D~%" m)
      (format t "Performing Forward Fast Walsh-Hadamard Transform...~%")
      (fwht c m mod-val)
      (format t "Applying power K=~D to transformed values...~%" k)
      (iterate (for i from 0 below m)
        (declare (type fixnum i))
        (setf (aref c i) (power-mod (aref c i) k mod-val)))
      (format t "Performing Inverse Fast Walsh-Hadamard Transform...~%")
      (inverse-fwht c m mod-val)
      (format t "Calculation complete. Formatting output...~%")
      (aref c 0))))


