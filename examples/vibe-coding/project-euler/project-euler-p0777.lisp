;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0777 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0777)

#||
Project Euler 777: Lissajous Curves
数論的ショートカットとメビウス反転を用いた O(N) 崩壊アルゴリズム
||#


(defmacro calculate-term-f (divisor limit)
  "基本の自己交差点の和 F(a,b) に対するメビウス反転の内部項を O(1) で計算する"
  `(let* ((max-index (floor ,limit ,divisor))
          (triangle-index (/ (* max-index (1+ max-index)) 2))
          (base-term (* ,divisor triangle-index)))
     (* base-term (- (* 2 base-term) (* 3 max-index)))))

(defmacro calculate-term-g1 (divisor limit)
  "特異点 G(a,b) のうち、a が 10 の倍数になるケースの内部項を O(1) で計算する"
  `(let* ((gcd-value (gcd ,divisor 10))
          (max-left (floor (* ,limit gcd-value) (* 10 ,divisor)))
          (max-right (floor ,limit ,divisor))
          (triangle-left (/ (* max-left (1+ max-left)) 2))
          (triangle-right (/ (* max-right (1+ max-right)) 2)))
     (+ (* -15 (/ (* ,divisor ,divisor) gcd-value) triangle-left triangle-right)
        (* (/ (* 15 ,divisor) (* 2 gcd-value)) triangle-left max-right)
        (* (/ (* 3 ,divisor) 4) max-left triangle-right)
        (* max-left max-right))))

(defmacro calculate-term-g3 (divisor limit)
  "特異点 G(a,b) のうち、a が 2 の倍数かつ b が 5 の倍数になるケースの内部項を O(1) で計算する"
  `(let* ((gcd-two (gcd ,divisor 2))
          (gcd-five (gcd ,divisor 5))
          (max-left (floor (* ,limit gcd-two) (* 2 ,divisor)))
          (max-right (floor (* ,limit gcd-five) (* 5 ,divisor)))
          (triangle-left (/ (* max-left (1+ max-left)) 2))
          (triangle-right (/ (* max-right (1+ max-right)) 2)))
     (+ (* -15 (/ (* ,divisor ,divisor) (* gcd-two gcd-five)) triangle-left triangle-right)
        (* (/ (* 3 ,divisor) (* 2 gcd-two)) triangle-left max-right)
        (* (/ (* 15 ,divisor) (* 4 gcd-five)) max-left triangle-right)
        (* max-left max-right))))

(defun compute-mobius (limit)
  "エラトステネスの篩を用いたメビウス関数の高速計算 O(N log log N)"
  (let ((mobius-array (make-array (1+ limit) :element-type 'fixnum :initial-element 0))
        (prime-flags (make-array (1+ limit) :element-type 'bit :initial-element 1)))
    (setf (aref mobius-array 1) 1)
    (setf (aref prime-flags 0) 0 (aref prime-flags 1) 0)
    (iterate (for index from 2 to limit)
      (when (= (aref prime-flags index) 1)
        (setf (aref mobius-array index) -1)
        (iterate (for multiple from (* 2 index) to limit by index)
          (setf (aref prime-flags multiple) 0)
          (setf (aref mobius-array multiple)
                (if (= (mod (floor multiple index) index) 0)
                    0
                    (- (aref mobius-array (floor multiple index))))))))
    mobius-array))

(defun format-scientific (number-value)
  "Project Eulerの要求フォーマット(10桁の有効数字をもつ科学的記数法)への安全な変換"
  (let* ((float-value (coerce number-value 'double-float)))
    (multiple-value-bind (sign-string mantissa exponent)
        (if (zerop number-value)
            (values "" 0.0d0 0)
            (let ((exponent-value (floor (log (abs float-value) 10d0))))
              (values (if (minusp float-value) "-" "")
                      (/ (abs number-value) (expt 10 exponent-value))
                      exponent-value)))
      (let* ((rounded-value (round (* mantissa 1000000000)))
             (mantissa-string (format nil "~10,'0D" rounded-value)))
        (if (>= rounded-value 10000000000)
            (format nil "~A~C.~Ae~D" sign-string (char mantissa-string 0) (subseq mantissa-string 1 10) (1+ exponent))
            (format nil "~A~C.~Ae~D" sign-string (char mantissa-string 0) (subseq mantissa-string 1 10) exponent))))))

(defun solve ()
  (let* ((limit 1000000)
         (mobius-array (compute-mobius limit)))
    
    (format t "Starting Mobius inversion loops for limit = ~D...~%" limit)
    (let ((sum-f 0)
          (sum-g-one 0)
          (sum-g-three 0))
      
      (iterate (for divisor from 1 to limit)
        (let ((mobius-value (aref mobius-array divisor)))
          (unless (zerop mobius-value)
            (incf sum-f (* mobius-value (calculate-term-f divisor limit)))
            (incf sum-g-one (* mobius-value (calculate-term-g1 divisor limit)))
            (incf sum-g-three (* mobius-value (calculate-term-g3 divisor limit))))))
      
      (format t "Loops finished. Applying boundary corrections...~%")
      ;; 境界補正（a=1 または b=1 の余剰項を除去する）
      (let* ((k-value (floor limit 10))
             (correction (/ (+ (* limit limit) (* -5 limit) 2 (* -15 k-value k-value) (* -8 k-value)) 2))
             (total-sum (- (+ sum-f (* 2 sum-g-one) (* 2 sum-g-three)) correction)))
        
        (format t "Calculation complete. Formatting output...~%")
        (format-scientific total-sum)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting Mobius inversion loops for limit = 1000000...
Loops finished. Applying boundary corrections...
Calculation complete. Formatting output...

User time    =        0.859
System time  =        0.025
Elapsed time =        0.804
Allocation   = 4418636160 bytes
3708 Page faults
GC time      =        0.009
 |------------------------------------------------------------|#
;;→ "2.533018434e23"
:ok