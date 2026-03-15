;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0196 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0196)

#||
(cl-text PE196-ALETHETIC-RESOLUTION
  (cl-comment "=============================================================================")
  (cl-comment " Project Euler 196: Alethetic Reset and ACX Jump ")
  (cl-comment "=============================================================================")

  (cl-comment "1. NMF (Non-Middle Fallacy) Avoidance")
  (cl-comment "Generating the entire triangle up to row 7,208,785 is an attachment to conventional truth (NMF).")
  (forall (?p ?a ?n)
    (if (and (Problem ?p)
             (= ?n 7208785)
             (Algorithm ?a)
             (Allocates_Full_Triangle ?a ?n))
        (and (NMF ?a)
             (attached_to_conventional_truth ?a))))

  (cl-comment "2. ACX Jump to Local Subspace")
  (cl-comment "The triplet condition strictly bounds the required context to depth +/- 2.")
  (forall (?a1)
    (if (NMF ?a1)
        (exists (?j ?a2)
          (and (ACX_Jump ?j)
               (target_of ?j ?a2)
               (Bounds_Context_To_Rows ?a2 (- ?n 2) (+ ?n 2))
               (uses_segmented_sieve ?a2)))))

  (cl-comment "3. Exact Integer Projection")
  (forall (?a)
    (if (OptimizedAlgorithm ?a)
        (and (eliminates_floating_point ?a)
             (uses_exact_integer_arithmetic ?a))))
)
||#


(defun triangle-value (row col)
  "指定された行・列の値を計算する。列は 0 <= col < row を満たす。"
  (+ (ash (* row (1- row)) -1) col 1))

(defun generate-base-primes (maximum-limit)
  "局所的な篩の種（Seed）となる勝義的な基底素数を生成する。"
  (let* ((sieve-array (make-array (1+ maximum-limit) :element-type 'bit :initial-element 0))
         (prime-vector (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0)))
    (setf (sbit sieve-array 0) 1
          (sbit sieve-array 1) 1)
    (iterate (for prime-candidate from 2 to maximum-limit)
      (when (= (sbit sieve-array prime-candidate) 0)
        (vector-push-extend prime-candidate prime-vector)
        (iterate (for multiple from (* prime-candidate prime-candidate) to maximum-limit by prime-candidate)
          (setf (sbit sieve-array multiple) 1))))
    prime-vector))

(defun build-segmented-sieve (start-value end-value base-primes)
  "ACX跳躍：必要な区間[start-value, end-value]のみを現成させる局所的篩（Segmented Sieve）"
  (let* ((interval-size (1+ (- end-value start-value)))
         (segment-sieve (make-array interval-size :element-type 'bit :initial-element 0)))
    (iterate (for base-prime in-vector base-primes)
      ;; 浮動小数点の丸め誤差による幻覚を排除するため、純粋な整数演算のみで境界を決定する
      (let* ((first-multiple (max (* base-prime base-prime)
                                  (* (ceiling start-value base-prime) base-prime))))
        (iterate (for multiple from first-multiple to end-value by base-prime)
          (setf (sbit segment-sieve (- multiple start-value)) 1))))
    segment-sieve))

(defun solve-row (target-row base-primes)
  "指定された行 target-row における Prime Triplet の合計値（空真理）を還元する。"
  (let* ((start-value (triangle-value (- target-row 2) 0))
         (end-value (triangle-value (+ target-row 2) (+ target-row 1)))
         (segment-sieve (build-segmented-sieve start-value end-value base-primes))
         (triplet-prime-sum 0))

    (labels ((is-prime-p (check-row check-col)
               (and (>= check-col 0)
                    (< check-col check-row)
                    (= (sbit segment-sieve (- (triangle-value check-row check-col) start-value)) 0)))

             (get-prime-neighbours (center-row center-col)
               (let ((prime-neighbours nil))
                 (iterate (for delta-row from -1 to 1)
                   (iterate (for delta-col from -1 to 1)
                     (when (not (and (= delta-row 0) (= delta-col 0)))
                       (let ((neighbour-row (+ center-row delta-row))
                             (neighbour-col (+ center-col delta-col)))
                         (when (is-prime-p neighbour-row neighbour-col)
                           (push (cons neighbour-row neighbour-col) prime-neighbours))))))
                 prime-neighbours)))

      (iterate (for current-col from 0 below target-row)
        (when (is-prime-p target-row current-col)
          (let ((immediate-neighbours (get-prime-neighbours target-row current-col)))
            ;; 条件1: 自身がTripletの中央である（2つ以上の素数隣接を持つ）
            ;; 条件2: 自身がTripletの端である（隣接する素数がさらに自分以外の素数隣接を持つ）
            (when (or (>= (length immediate-neighbours) 2)
                      (and (= (length immediate-neighbours) 1)
                           (let* ((sole-neighbour (first immediate-neighbours))
                                  (extended-neighbours (get-prime-neighbours (car sole-neighbour) (cdr sole-neighbour))))
                             (>= (length extended-neighbours) 2))))
              (incf triplet-prime-sum (triangle-value target-row current-col)))))))
    triplet-prime-sum))

(defun solve ()
  (format t "Initializing Alethetic Base Primes...~%")
  (let* ((target-row-1 5678027)
         (target-row-2 7208785)
         ;; 必要な最大値を正確に算出し、isqrtによって浮動小数点を排除する
         (maximum-value (triangle-value (+ target-row-2 2) (+ target-row-2 1)))
         (base-prime-limit (isqrt maximum-value))
         (base-primes (generate-base-primes base-prime-limit)))
    (format t "Base Primes Manifested. Count: ~A~%" (length base-primes))
    
    (let ((sum-1 (solve-row target-row-1 base-primes)))
      (format t "S(~A) = ~A~%" target-row-1 sum-1)
      (let ((sum-2 (solve-row target-row-2 base-primes)))
        (format t "S(~A) = ~A~%" target-row-2 sum-2)
        (let ((total-sum (+ sum-1 sum-2)))
          (format t "Total Sum = ~A~%" total-sum)
          total-sum)))))


