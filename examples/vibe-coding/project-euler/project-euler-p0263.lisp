;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0263 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0263)

#||
(cl-text https://projecteuler.net/problem=263
(cl-comment "Project Euler 263: An engineers' paradise")
(cl-comment "A number n is an engineer's paradise if:")
(cl-comment "1. n-9, n-3, n+3, n+9 are consecutive primes.")
(cl-comment "2. n-8, n-4, n, n+4, n+8 are all practical numbers.")
(cl-comment "Mathematical jump: To satisfy practical conditions, n MUST be 4 mod 8.")
(cl-comment "Primes condition forces n = 0 mod 5, n != 0 mod 3, n = 0,1,6 mod 7.")
(cl-comment "Applying Chinese Remainder Theorem forces n = 20 or 820 mod 840.")
(forall (n)
        (implies (EngineerParadise n)
                 (or (Equal (Mod n 840) 20)
                     (Equal (Mod n 840) 820))))
)
||#

(defun pow-mod (base exp mod-val)
  "Bignumアロケーションを避ける高速なべき乗剰余"
  (declare (type fixnum base exp mod-val))
  (let ((res 1)
        (b (mod base mod-val)))
    (declare (type fixnum res b))
    (loop while (> exp 0) do
      (when (oddp exp)
        (setf res (mod (* res b) mod-val)))
      (setf exp (ash exp -1))
      (setf b (mod (* b b) mod-val)))
    res))

(defun miller-rabin (n)
  "決定的ミラー・ラビン素数判定法 (n < 4.7 * 10^9 の範囲で完全な精度)"
  (declare (type fixnum n))
  (if (< n 2) (return-from miller-rabin nil))
  (if (or (= n 2) (= n 3) (= n 5) (= n 7)) (return-from miller-rabin t))
  (if (evenp n) (return-from miller-rabin nil))
  (let ((d (1- n))
        (s 0))
    (declare (type fixnum d s))
    (loop while (evenp d) do
      (setf d (ash d -1))
      (incf s))
    (dolist (a '(2 7 61))
      (when (< a n)
        (let ((x (pow-mod a d n))
              (composite t))
          (declare (type fixnum x))
          (if (or (= x 1) (= x (1- n)))
              (setf composite nil)
              (loop for r from 1 below s do
                (setf x (mod (* x x) n))
                (when (= x (1- n))
                  (setf composite nil)
                  (return))))
          (when composite
            (return-from miller-rabin nil)))))
    t))

(defun is-practical (n)
  "実用数(Practical number)の判定"
  (declare (type fixnum n))
  (when (oddp n) (return-from is-practical nil))
  (let ((current-sigma 1)
        (temp n))
    (declare (type fixnum current-sigma temp))
    ;; 素因数 2 の抽出
    (let ((count 0))
      (declare (type fixnum count))
      (loop while (evenp temp) do
        (incf count)
        (setf temp (ash temp -1)))
      (setf current-sigma (1- (ash 1 (1+ count)))))
    
    ;; 奇素因数の抽出と判定
    (let ((p 3))
      (declare (type fixnum p))
      (loop while (<= (* p p) temp) do
        (when (= (mod temp p) 0)
          ;; p_i <= 1 + sigma(p_1...p_i-1) を満たさなければ実用数ではない
          (if (> p (1+ current-sigma))
              (return-from is-practical nil))
          (let ((p-pow p))
            (declare (type fixnum p-pow))
            (loop while (= (mod temp p) 0) do
              (setf temp (truncate temp p))
              (setf p-pow (* p-pow p)))
            (setf current-sigma (* current-sigma (truncate (1- p-pow) (1- p))))
            ;; オーバーフロー防止（nを超えれば条件は常にクリアされる）
            (when (> current-sigma n) (setf current-sigma n))))
        (incf p 2))
      
      ;; 最後に残った素因数のチェック
      (when (> temp 1)
        (if (> temp (1+ current-sigma))
            (return-from is-practical nil))))
    t))

(defun check-engineer-paradise (n)
  "Engineer's Paradise の全条件をショートサーキットで評価"
  (declare (type fixnum n))
  ;; 1. 指定された4つが素数であるか (ここでの脱落が99%以上)
  (unless (miller-rabin (- n 9)) (return-from check-engineer-paradise nil))
  (unless (miller-rabin (- n 3)) (return-from check-engineer-paradise nil))
  (unless (miller-rabin (+ n 3)) (return-from check-engineer-paradise nil))
  (unless (miller-rabin (+ n 9)) (return-from check-engineer-paradise nil))
  
  ;; 2. それらが「連続する」素数であるか（間に別の素数がないか）
  (when (miller-rabin (- n 7)) (return-from check-engineer-paradise nil))
  (when (miller-rabin (- n 5)) (return-from check-engineer-paradise nil))
  (when (miller-rabin (- n 1)) (return-from check-engineer-paradise nil))
  (when (miller-rabin (+ n 1)) (return-from check-engineer-paradise nil))
  (when (miller-rabin (+ n 5)) (return-from check-engineer-paradise nil))
  (when (miller-rabin (+ n 7)) (return-from check-engineer-paradise nil))
  
  ;; 3. 実用数のチェック
  (unless (is-practical (- n 8)) (return-from check-engineer-paradise nil))
  (unless (is-practical (- n 4)) (return-from check-engineer-paradise nil))
  (unless (is-practical n) (return-from check-engineer-paradise nil))
  (unless (is-practical (+ n 4)) (return-from check-engineer-paradise nil))
  (unless (is-practical (+ n 8)) (return-from check-engineer-paradise nil))
  
  t)

(defun solve ()
  (let ((found 0)
        (sum 0))
    (declare (type fixnum found sum))
    (format t "Starting search with step size 840...~%")
    (iterate (for k from 1)
             ;; 数学的に証明された 2つの合同類 mod 840 のみをテスト
             (let ((n1 (- (* 840 k) 20))
                   (n2 (+ (* 840 k) 20)))
               (declare (type fixnum n1 n2))
               
               (when (check-engineer-paradise n1)
                 (format t "Found Paradise: ~a~%" n1)
                 (incf sum n1)
                 (incf found)
                 (when (= found 4) (return-from solve sum)))
               
               (when (check-engineer-paradise n2)
                 (format t "Found Paradise: ~a~%" n2)
                 (incf sum n2)
                 (incf found)
                 (when (= found 4) (return-from solve sum)))))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting search with step size 840...
Found Paradise: 219869980
Found Paradise: 312501820
Found Paradise: 360613700
Found Paradise: 1146521020

User time    =        4.997
System time  =        0.051
Elapsed time =        4.965
Allocation   = 5097512 bytes
3884 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 2039506520
:ok
