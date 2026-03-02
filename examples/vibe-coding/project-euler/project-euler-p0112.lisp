;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: chatgpt
(cl:in-package cl-user)
(defpackage #:project-euler-0112 (:use cl #:alexandria #:iterate))
(in-package #:project-euler-0112)

#||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Logic (CLIF) Analysis of Project Euler Problem 112
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-module project-euler-0112

  ;; 基本領域
  (forall (n)
    (if (positive-integer n)
        (and (>= n 1))))

  ;; 桁列定義
  (forall (n d)
    (if (digit-sequence-of d n)
        (and (sequence d)
             (forall (i)
               (if (and (index i d)
                        (< i (last-index d)))
                   (digit (at d i)))))))

  ;; increasing 定義
  (forall (n d)
    (if (digit-sequence-of d n)
        (iff (increasing n)
             (forall (i)
               (if (< i (last-index d))
                   (<= (at d i) (at d (+ i 1))))))))

  ;; decreasing 定義
  (forall (n d)
    (if (digit-sequence-of d n)
        (iff (decreasing n)
             (forall (i)
               (if (< i (last-index d))
                   (>= (at d i) (at d (+ i 1))))))))

  ;; bouncy 定義
  (forall (n)
    (iff (bouncy n)
         (and (positive-integer n)
              (not (increasing n))
              (not (decreasing n)))))

  ;; 比率定義
  (forall (n)
    (iff (bouncy-ratio-99 n)
         (and (= (* 100 (count-bouncy-up-to n))
                 (* 99 n))
              (minimal n
                (= (* 100 (count-bouncy-up-to n))
                   (* 99 n))))))

)
||#


(defun digits-of (n)
  (coerce (map 'list (lambda (c) (- (char-code c) (char-code #\0)))
               (write-to-string n))
          'list))

(defun increasing-p (n)
  (let ((digits (digits-of n)))
    (iterate
      (for (a b) on digits)
      (while b)
      (always (<= a b)))))

(defun decreasing-p (n)
  (let ((digits (digits-of n)))
    (iterate
      (for (a b) on digits)
      (while b)
      (always (>= a b)))))

(defun bouncy-p (n)
  (and (not (increasing-p n))
       (not (decreasing-p n))))

(defun solve ()
  (iterate
    (for n from 1)
    (with bouncy-count = 0)
    (when (bouncy-p n)
      (incf bouncy-count))
    (when (and (>= n 100)
               (= (* 100 bouncy-count)
                  (* 99 n)))
      (return n))))

;; 実行用
#|
(solve)
|#
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        5.330
System time  =        0.021
Elapsed time =        5.323
Allocation   = 480564640 bytes
269 Page faults
GC time      =        0.003
 |------------------------------------------------------------|#
;;→ 1587000
