;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0486 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0486)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defconstant $modulus 87654321)

(defun is-pal5 (x)
  (and (= (ash x -4) (logand x 1))
       (= (logand (ash x -3) 1) (logand (ash x -1) 1))))

(defun is-pal6 (x)
  (and (= (ash x -5) (logand x 1))
       (= (logand (ash x -4) 1) (logand (ash x -1) 1))
       (= (logand (ash x -3) 1) (logand (ash x -2) 1))))

(defun extended-gcd (a b)
  (if (= a 0)
      (values b 0 1)
      (multiple-value-bind (g x y) (extended-gcd (mod b a) a)
        (values g (- y (* (floor b a) x)) x))))

(defun mod-inverse (a m)
  (multiple-value-bind (g x y) (extended-gcd a m)
    (declare (ignore y))
    (if (= g 1)
        (mod x m)
        (error "No inverse"))))

(defun get-order (m)
  (let ((val 2)
        (ord 1))
    (iterate ((_ (scan-range :from 0)))
      (when (= val 1) (terminate-producing))
      (setf val (mod (* val 2) m))
      (incf ord))
    ord))

(defun precompute-solver (A M)
  "A * x ≡ B (mod M) を解くための不変量を事前計算する"
  (let* ((g (gcd A M))
         (M0 (floor M g))
         (A0 (floor A g)))
    (values g M0 (mod-inverse A0 M0))))

(defun count-valid-q (q-max q0 q-lcm)
  "0 <= q <= q-max の範囲で q ≡ q0 (mod q-lcm) となる q の個数をO(1)で計算する"
  (if (< q-max 0) 0
      (let ((max-k (floor (- q-max q0) q-lcm))
            (min-k (ceiling (- 0 q0) q-lcm)))
        (max 0 (1+ (- max-k min-k))))))

(defun solve-for (limit)
  (let ((curr (make-array 32 :element-type 'fixnum :initial-element 0))
        (next (make-array 32 :element-type 'fixnum :initial-element 0))
        (G-vals (make-array 300 :element-type 'integer :initial-element 0))
        (S-vals (make-array 300 :element-type 'integer :initial-element 0)))
    
    ;; 1. DFAのシミュレーションと G(n) の計算
    (setf (aref G-vals 1) 2 (aref G-vals 2) 4 (aref G-vals 3) 8 (aref G-vals 4) 16)
    (iterate ((i (scan-range :from 0 :below 32)))
      (unless (is-pal5 i) (setf (aref curr i) 1)))
    (setf (aref G-vals 5) 24)
    
    (iterate ((n (scan-range :from 6 :upto 250)))
      (fill next 0)
      (iterate ((i (scan-range :from 0 :below 32)))
        (when (> (aref curr i) 0)
          (iterate ((bit (scan-range :from 0 :below 2)))
            (let ((x6 (logior (ash i 1) bit)))
              (unless (or (is-pal6 x6) (is-pal5 (logand x6 31)))
                (incf (aref next (logand x6 31)) (aref curr i)))))))
      (let ((sum 0))
        (iterate ((i (scan-range :from 0 :below 32))) (incf sum (aref next i)))
        (setf (aref G-vals n) sum))
      (replace curr next))
      
    ;; 累積和 S(n)
    (iterate ((n (scan-range :from 1 :upto 250)))
      (setf (aref S-vals n) (+ (aref S-vals (1- n)) (aref G-vals n))))
      
    ;; 2. 周期 P の動的発見（定数への収束という幻覚を捨てる）
    (let ((P nil))
      (iterate ((p-test (scan-range :from 1 :upto 100)))
        (let ((ok t))
          (iterate ((i (scan-range :from 100 :upto 150)))
            (when (/= (aref G-vals i) (aref G-vals (+ i p-test)))
              (setf ok nil)
              (terminate-producing)))
          (when ok
            (setf P p-test)
            (terminate-producing))))
      (unless P (error "Fatal: No periodic behavior found!"))
      
      (let* ((T-thresh 100) ; 過渡期を確実に抜けるための安全マージン
             (Delta (- (aref S-vals (+ T-thresh P)) (aref S-vals T-thresh)))
             (M $modulus)
             (O (get-order M))
             (total-zeros 0))
        
        ;; 3. 過渡期 [5, T] の直接カウント
        (iterate ((n (scan-range :from 5 :upto T-thresh)))
          (when (<= n limit)
            (let ((F5 (- (ash 1 (1+ n)) 2 (aref S-vals n))))
              (when (= (mod F5 M) 0)
                (incf total-zeros)))))
                
        ;; 4. 周期期 [T+1, Limit] の O(1) CRT跳躍カウント
        ;; 演算のホイスティング（事前計算）による究極の最適化
        (multiple-value-bind (gM M-prime invM) (precompute-solver Delta M)
          (multiple-value-bind (gO O-prime invO) (precompute-solver P O)
            (multiple-value-bind (g-crt u-crt v-crt) (extended-gcd M-prime O-prime)
              (declare (ignore v-crt))
              (let ((lcm-crt (* (floor M-prime g-crt) O-prime)))
                (iterate ((r (scan-range :from 0 :below P)))
                  (let* ((S-rem (- (aref S-vals (+ T-thresh 1 r)) (aref S-vals T-thresh)))
                         (Cr (+ 2 (aref S-vals T-thresh) S-rem))
                         (R 2))
                    (iterate ((n0 (scan-range :from 0 :below O)))
                      (let ((diff-M (mod (- R Cr) M)))
                        (when (= (mod diff-M gM) 0)
                          (let* ((B0-M (floor diff-M gM))
                                 (qM (mod (* B0-M invM) M-prime)))
                            (let ((diff-O (mod (- n0 T-thresh 1 r) O)))
                              (when (= (mod diff-O gO) 0)
                                (let* ((B0-O (floor diff-O gO))
                                       (qO (mod (* B0-O invO) O-prime)))
                                  (when (= (mod (- qO qM) g-crt) 0)
                                    (let* ((diff-crt (floor (- qO qM) g-crt))
                                           (q-ans (mod (+ qM (* u-crt diff-crt M-prime)) lcm-crt))
                                           (q-max (floor (- limit T-thresh 1 r) P)))
                                      (incf total-zeros (count-valid-q q-max q-ans lcm-crt))))))))))
                      (setf R (mod (* R 2) M)))))))))
        total-zeros))))

(defun solve ()
  (format t "Verifying D(10^7) = ~A (Expected: 0)~%" (solve-for 10000000))
  (format t "Verifying D(5 * 10^9) = ~A (Expected: 51)~%" (solve-for 5000000000))
  (let ((ans (solve-for #.(expt 10 18))))
    (format t "Final Answer for D(10^18) = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0486:solve)