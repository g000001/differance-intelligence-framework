
;;; -*- mode: Lisp; coding: utf-8  -*-
(cl:in-package cl-user)
(defpackage #:project-euler-0971
  (:use #:cl #:iterate))
(in-package #:project-euler-0971)

;; べき剰余計算
(declaim (inline pow-mod))
(defun pow-mod (base exp m)
  (declare (type (unsigned-byte 64) base exp m))
  (let ((res 1)
        (b (mod base m)))
    (declare (type (unsigned-byte 64) res b))
    (iter (while (> exp 0))
          (when (oddp exp)
            (setf res (mod (* res b) m)))
          (setf b (mod (* b b) m))
          (setf exp (ash exp -1)))
    res))

;; 写像 h のサイクルに含まれる要素数をカウント
(defun count-cycle-elements (h)
  (declare (type (simple-array fixnum (5)) h))
  (iter (for i from 0 to 4)
        (counting
         (iter (for j from 1 to 5)
               (for curr initially i then (aref h curr))
               (thereis (= (aref h curr) i))))))

(defun solve (&optional (n 100000000))
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 64) n))
  (let ((sieve (make-array (1+ n) :element-type 'bit :initial-element 1))
        (total-sum 0))
    (declare (type (unsigned-byte 64) total-sum))
    ;; 素数篩
    (setf (aref sieve 0) 0)
    (setf (aref sieve 1) 0)
    (let ((limit (isqrt n)))
      (iter (for i from 2 to limit)
            (when (= (aref sieve i) 1)
              (iter (for j from (* i i) to n by i)
                    (setf (aref sieve j) 0)))))
    
    ;; p = 5k - 4 かつ p <= N の素数について集計
    ;; p = 11, 31, 41, ... (p=5は含まれず、以降 p ≡ 1 mod 10)
    (iter (for p from 11 to n by 10)
          (declare (type (unsigned-byte 64) p))
          (when (= (aref sieve p) 1)
            (let* ((exp (floor (- p 1) 5))
                   ;; 1の原始5乗根を見つける
                   (zeta (iter (for x from 2)
                               (let ((r (pow-mod x exp p)))
                                 (when (/= r 1) (return r)))))
                   (roots (make-array 5 :element-type '(unsigned-byte 64))))
              (declare (type (unsigned-byte 64) exp zeta))
              ;; 5乗根のリスト生成
              (setf (aref roots 0) 1)
              (iter (for i from 1 to 4)
                    (setf (aref roots i) (mod (* (aref roots (1- i)) zeta) p)))
              
              ;; 写像 h の構築
              (let ((h (make-array 5 :element-type 'fixnum)))
                (iter (for a from 0 to 4)
                      (let* ((y (mod (1+ (aref roots a)) p))
                             (z (pow-mod y exp p))
                             (b (position z roots)))
                        (setf (aref h a) (mod (+ a b) 5))))
                
                ;; C(p) = 1 + (p-1)/5 * nc
                (let ((nc (count-cycle-elements h)))
                  (incf total-sum (the (unsigned-byte 64) (+ 1 (* exp nc)))))))))
    total-sum))

;; 計算の実行
;; (print (solve 100000000))

#+| Do it | (solve 100000000)
;→ 33626723890930

:ok