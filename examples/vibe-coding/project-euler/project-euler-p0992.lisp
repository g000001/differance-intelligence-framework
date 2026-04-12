;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0992 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0992)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(defconstant $modulo-constant 987898789)

(defparameter *factorial-array* (make-array 30000 :initial-element 1))
(defparameter *inverse-factorial-array* (make-array 30000 :initial-element 1))

(defun power-mod (base exponent)
  (iterate
    (with result = 1)
    (with current-base = (mod base $modulo-constant))
    (with current-exp = exponent)
    (while (> current-exp 0))
    (when (oddp current-exp)
      (setf result (mod (* result current-base) $modulo-constant)))
    (setf current-base (mod (* current-base current-base) $modulo-constant))
    (setf current-exp (ash current-exp -1))
    (finally (return result))))

(defun modular-inverse (number)
  (power-mod number (- $modulo-constant 2)))

(defun precompute-factorials ()
  (iterate
    (for index from 1 below 30000)
    (setf (aref *factorial-array* index)
          (mod (* (aref *factorial-array* (1- index)) index) $modulo-constant)))
  (setf (aref *inverse-factorial-array* 29999)
        (modular-inverse (aref *factorial-array* 29999)))
  (iterate
    (for index from 29998 downto 0)
    (setf (aref *inverse-factorial-array* index)
          (mod (* (aref *inverse-factorial-array* (1+ index)) (1+ index)) $modulo-constant))))

(defun combinations (total choose)
  (if (or (< choose 0) (> choose total))
      0
      (let ((numerator (aref *factorial-array* total))
            (denominator1 (aref *inverse-factorial-array* choose))
            (denominator2 (aref *inverse-factorial-array* (- total choose))))
        (mod (* numerator (mod (* denominator1 denominator2) $modulo-constant)) $modulo-constant))))

(defun compute-j (num-stones limit-k)
  (iterate
    (for end-stone from 0 to num-stones)
    (with total-ways = 0)
    (let ((valid-path-p t)
          (ways-for-end 1)
          (l-array (make-array (1+ num-stones) :initial-element 0))
          (r-array (make-array (1+ num-stones) :initial-element 0))
          (c-array (make-array (1+ num-stones) :initial-element 0)))

      ;; 境界値における不変量 C_i の固定
      (iterate
        (for stone-idx from 0 below num-stones)
        (setf (aref c-array stone-idx) (if (< stone-idx end-stone) 1 0)))

      ;; 漸化式による純粋な整数還元
      (setf (aref l-array 1) (- limit-k 1))
      (iterate
        (for stone-idx from 1 below num-stones)
        (setf (aref l-array (1+ stone-idx))
              (- (+ limit-k stone-idx)
                 (aref l-array stone-idx)
                 (aref c-array (1- stone-idx)))))

      (iterate
        (for stone-idx from 0 below num-stones)
        (setf (aref r-array stone-idx)
              (+ (aref l-array (1+ stone-idx))
                 (aref c-array stone-idx))))
      (setf (aref r-array num-stones) 0)

      ;; 不全（負数）の検知による自己批判
      (iterate
        (for stone-idx from 0 to num-stones)
        (when (or (< (aref l-array stone-idx) 0)
                  (< (aref r-array stone-idx) 0))
          (setf valid-path-p nil)))

      (when valid-path-p
        (iterate
          (for stone-idx from 0 to num-stones)
          (let ((r-val (aref r-array stone-idx))
                (l-val (aref l-array stone-idx)))
            (cond
              ;; 訪問しない閉包領域は1通りとして乗算に影響させない
              ((and (= r-val 0) (= l-val 0)))
              
              ;; 終点より左側（最後の移動は必ず右）
              ((< stone-idx end-stone)
               (if (= r-val 0)
                   (setf ways-for-end 0)
                   (setf ways-for-end (mod (* ways-for-end (combinations (1- (+ r-val l-val)) l-val)) $modulo-constant))))
              
              ;; 終点より右側（最後の移動は必ず左）
              ((> stone-idx end-stone)
               (if (= l-val 0)
                   (setf ways-for-end 0)
                   (setf ways-for-end (mod (* ways-for-end (combinations (1- (+ r-val l-val)) r-val)) $modulo-constant))))
              
              ;; 終点 E（観測者の固定点・制限なしの全単射）
              (t 
               (setf ways-for-end (mod (* ways-for-end (combinations (+ r-val l-val) r-val)) $modulo-constant)))))))

      (when valid-path-p
        (setf total-ways (mod (+ total-ways ways-for-end) $modulo-constant))))
    (finally (return total-ways))))

(defun solve ()
  (precompute-factorials)
  (let ((total-sum 0)
        (num-stones 500))
    (iterate
      (for power-s from 0 to 4)
      (let* ((limit-k (expt 10 power-s))
             (sub-answer (compute-j num-stones limit-k)))
        ;; 観測用プリントデバッグ
        (format t "[DEBUG] s=~A, J(~A, ~A) = ~A~%" power-s num-stones limit-k sub-answer)
        (setf total-sum (mod (+ total-sum sub-answer) $modulo-constant))))
    total-sum))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[DEBUG] s=0, J(500, 1) = 197475864
[DEBUG] s=1, J(500, 10) = 96349339
[DEBUG] s=2, J(500, 100) = 180095288
[DEBUG] s=3, J(500, 1000) = 534311939
[DEBUG] s=4, J(500, 10000) = 547687593

User time    =        0.124
System time  =        0.011
Elapsed time =        0.077
Allocation   = 30818568 bytes
3852 Page faults
GC time      =        0.003
 |------------------------------------------------------------|#
;;→ 568021234
:ok