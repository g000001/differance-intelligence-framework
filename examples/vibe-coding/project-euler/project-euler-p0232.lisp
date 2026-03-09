;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0232 (:use cl iterate alexandria))
(in-package #:project-euler-0232)

#||
(cl-text euler-p232-acx
  (cl-comment "Ontology for Project Euler P232: The Race")
  (cl-comment "Applying Two-Truths Entanglement to resolve infinite stochastic loops")

  (forall (x y)
    (iff (GameState x y)
         (and (Integer x) (Integer y)
              (<= 0 x) (<= 0 y))))

  (cl-comment "Conventional Truth (世俗諦): Infinite probabilistic transitions")
  (forall (x y T)
    (iff (Transitions x y T)
         (and (= (ProbSuccess T) (/ 1 (expt 2 T)))
              (= (ScoreIncrement T) (expt 2 (- T 1))))))

  (cl-comment "ACX Jump (跳躍 ρ): Algebraic reduction of infinite loops")
  (cl-comment "Resolves the self-referential DP into a strict directed acyclic evaluation")
  (forall (P W x y T)
    (iff (UltimateValue P x y T)
         (= (P x y)
            (/ (+ (P x (+ y (expt 2 (- T 1))))
                  (* (expt 2 T) (W (+ x 1) y)))
               (+ (expt 2 T) 1)))))

  (cl-comment "Middle Way (中道): The fixed-point DP computation")
  (forall (P W x y)
    (iff (OptimalStrategy P W x y)
         (and (= (P x y) (max_T (UltimateValue P x y T)))
              (= (W x y) (- (* 2 (P x y)) (W (+ x 1) y))))))
)
||#


(defun solve ()
  (let ((p-table (make-array '(101 101) :element-type 'double-float :initial-element 0.0d0))
        (w-table (make-array '(101 101) :element-type 'double-float :initial-element 0.0d0)))
    
    (iterate (for x from 99 downto 0)
      (iterate (for y from 99 downto 0)
        (let ((max-p 0.0d0))
          ;; Player 2 can choose T >= 1. 
          ;; 2^(8-1) = 128, which is always enough to reach or exceed 100 points.
          (iterate (for t-val from 1 to 8)
            (let* ((y-next (+ y (ash 1 (1- t-val))))
                   ;; If y reaches or exceeds 100, Player 2 wins instantly (prob 1.0)
                   (p-next (if (>= y-next 100) 1.0d0 (aref p-table x y-next)))
                   ;; If x reaches or exceeds 100, Player 1 has already won (prob 0.0 for P2)
                   (w-next (if (>= (1+ x) 100) 0.0d0 (aref w-table (1+ x) y)))
                   (pow2 (float (ash 1 t-val) 0.0d0))
                   ;; DP relation resolving the infinite loop algebraically
                   (val (/ (+ p-next (* pow2 w-next)) (+ pow2 1.0d0))))
              (if (> val max-p)
                  (setf max-p val))))
          
          (setf (aref p-table x y) max-p)
          ;; W(x, y) = 2 * P(x, y) - W(x+1, y) derived from the starting state
          (setf (aref w-table x y) (- (* 2.0d0 max-p)
                                      (if (>= (1+ x) 100) 0.0d0 (aref w-table (1+ x) y)))))))
    
    ;; We want the probability Player 2 wins from score (0,0) at the start of Player 1's turn
    (format nil "0.~8,'0D" (round (* (aref p-table 0 0) 100000000.0d0)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)

User time    =        0.015
System time  =        0.000
Elapsed time =        0.016
Allocation   = 9226264 bytes
37 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "0.83648556"
:ok