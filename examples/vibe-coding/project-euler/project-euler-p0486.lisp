;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0486 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0486)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defun extended-gcd (a b)
  (declare (type integer a b))
  (if (= a 0)
      (values b 0 1)
      (multiple-value-bind (g x y) (extended-gcd (mod b a) a)
        (values g (- y (* (floor b a) x)) x))))

(defun mod-inverse (a m)
  (declare (type integer a m))
  (multiple-value-bind (g x y) (extended-gcd a m)
    (declare (ignore y))
    (if (= g 1)
        (mod x m)
        nil)))

(defun pal-p (x len)
  "2進数表記での回文判定"
  (let ((s (format nil "~v,'0B" len x)))
    (string= s (reverse s))))

(defconstant +M+ 87654321)
(defconstant +L+ 1000000000000000000)

(defun solve ()
  (let* ((M +M+)
         (L +L+)
         (dp (make-array '(200 32) :initial-element 0))
         (g-arr (make-array 200 :initial-element 0))
         (sum-g (make-array 200 :initial-element 0))
         (F5 (make-array 200 :initial-element 0)))
         
    (format t "Step 1: 状態遷移マトリクスと g(n) 系列の生成...~%")
    ;; n=5 の初期化
    (dotimes (i 32)
      (unless (pal-p i 5)
        (setf (aref dp 5 i) 1)))
        
    ;; n > 5 の遷移
    (loop for n from 6 to 100 do
      (dotimes (i 32)
        (unless (pal-p i 5)
          (dotimes (bit 2)
            (let ((nxt (mod (+ (* i 2) bit) 32))
                  (six (+ (* i 2) bit)))
              (unless (or (pal-p nxt 5) (pal-p six 6))
                (incf (aref dp n nxt) (aref dp (- n 1) i))))))))
                
    ;; g(n), 累積和, F_5(n) の計算
    (dotimes (n 100)
      (if (< n 5)
          (setf (aref g-arr n) (ash 1 n))
          (dotimes (i 32)
            (incf (aref g-arr n) (aref dp n i))))
      (setf (aref sum-g n) (if (= n 0) (aref g-arr 0) (+ (aref sum-g (- n 1)) (aref g-arr n))))
      (setf (aref F5 n) (- (ash 1 (+ n 1)) 1 (aref sum-g n))))
      
    (format t "  [Debug] F_5(5) = ~A (Expected: 8)~%" (aref F5 5))
    (format t "  [Debug] F_5(6) = ~A (Expected: 42)~%" (aref F5 6))
    (format t "  [Debug] F_5(11) = ~A (Expected: 3844)~%" (aref F5 11))
    
    (format t "Step 2: 周期性の抽出とCRT用の定数事前計算...~%")
    (let* ((T-period 6)
           (N0 20) ; 過渡状態を確実に抜けるためのオフセット
           (S-sum (loop for i from 1 to T-period sum (aref g-arr (+ N0 i))))
           (P (let ((val 2) (p-ord 1))
                (loop while (/= val 1) do
                  (setf val (mod (* val 2) M))
                  (incf p-ord))
                p-ord))
           (g-gcd (gcd S-sum M))
           (S-prime (/ S-sum g-gcd))
           (M-prime (/ M g-gcd))
           (inv-S (mod-inverse S-prime M-prime))
           (M2 (* T-period M-prime))
           (gcd-M1-M2 (gcd P M2))
           (M2-prime (/ M2 gcd-M1-M2))
           (lcm-M (* P M2-prime))
           (inv-P (mod-inverse (/ P gcd-M1-M2) M2-prime))
           (gcd-PT (gcd P T-period))
           (ans 0))
           
      (format t "  [Info] S_sum = ~A, P (ord_M(2)) = ~A~%" S-sum P)
      
      ;; 境界値 (n < N0) のチェック
      (loop for n from 5 below N0 do
        (when (= (mod (aref F5 n) M) 0)
          (incf ans)))
          
      (format t "Step 3: O(P) による CRT 連立合同式の求解...~%")
      (let ((C 2))
        (dotimes (x P)
          (dotimes (r T-period)
            (let ((n-mod-T (mod (+ N0 r) T-period)))
              ;; x ≡ N_0 + r (mod gcd(P, T)) の前提条件
              (when (= (mod x gcd-PT) (mod n-mod-T gcd-PT))
                (let ((V (mod (- C 1 (aref sum-g (+ N0 r))) M)))
                  (when (= (mod V g-gcd) 0)
                    (let* ((V-prime (floor V g-gcd))
                           (q0 (mod (* V-prime inv-S) M-prime))
                           (n1 (+ N0 r (* q0 T-period))))
                      ;; CRTの解の存在条件
                      (when (= (mod x gcd-M1-M2) (mod n1 gcd-M1-M2))
                        (let* ((diff (floor (- n1 x) gcd-M1-M2))
                               (k (mod (* diff inv-P) M2-prime))
                               (n-crt (mod (+ x (* k P)) lcm-M)))
                          (let ((start n-crt))
                            ;; n-crt が N0 に達するまで lcm-M を加算して正規化
                            (when (< start N0)
                              (incf start (* (ceiling (- N0 start) lcm-M) lcm-M)))
                            ;; L以下の解の個数を計算
                            (when (<= start L)
                              (incf ans (1+ (floor (- L start) lcm-M)))))))))))))
          (setf C (mod (* C 2) M))))
      
      (format t "Final Result D(~A): ~A~%" L ans)
      ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: 状態遷移マトリクスと g(n) 系列の生成...
  [Debug] F_5(5) = 8 (Expected: 8)
  [Debug] F_5(6) = 42 (Expected: 42)
  [Debug] F_5(11) = 3844 (Expected: 3844)
Step 2: 周期性の抽出とCRT用の定数事前計算...
  [Info] S_sum = 200, P (ord_M(2)) = 7299372
Step 3: O(P) による CRT 連立合同式の求解...
Final Result D(1000000000000000000): 11408450515

User time    =        2.560
System time  =        0.030
Elapsed time =        2.502
Allocation   = 2919104 bytes
3916 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 11408450515
:ok