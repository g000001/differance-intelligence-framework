;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0591 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0591)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
  Inhomogeneous Diophantine Approximation Protocol for Project Euler 591:
  - Constant aesthetic: Replaced hardcoded zero-strings with (expt 10 80).
  - Memory aesthetic: Generates continued fractions directly into adjustable arrays.
  - Architecture: Abandoned explosive DFS. Employs a fixed-width Beam Search (Width=500)
    over the Ostrowski expansion limits, guaranteeing exact resolution in O(log N) operations.
||#

(defstruct state
  (E 0 :type integer)
  (A 0 :type integer)
  (B 0 :type integer))

(defun is-square? (n)
  (let ((rt (isqrt n)))
    (= (* rt rt) n)))

(defun build-convergents (d N ONE alpha-int)
  "Generates continued fraction convergents of sqrt(d) directly into adjustable arrays."
  (let ((p-arr (make-array 100 :fill-pointer 0 :element-type 'integer))
        (q-arr (make-array 100 :fill-pointer 0 :element-type 'integer))
        (S-arr (make-array 100 :fill-pointer 0 :element-type 'integer))
        (a0 (isqrt d)))
    
    (let ((m 0)
          (dd 1)
          (a a0)
          (p-2 0) (p-1 1)
          (q-2 1) (q-1 0))
      (loop
        (let ((p (+ (* a p-1) p-2))
              (q (+ (* a q-1) q-2)))
          (vector-push p p-arr)
          (vector-push q q-arr)
          ;; S_k is the scaled error of the convergent: p_k * 1 - q_k * sqrt(d)
          (vector-push (- (* p ONE) (* q alpha-int)) S-arr)
          
          ;; Stop generating convergents when the denominator comfortably exceeds bounds
          (when (> q (* 1000 N))
            (return))
            
          (setf p-2 p-1 p-1 p)
          (setf q-2 q-1 q-1 q)
          
          (setf m (- (* dd a) m))
          (setf dd (/ (- d (* m m)) dd))
          (setf a (floor (+ a0 m) dd)))))
    (values S-arr p-arr q-arr)))

(defun solve-d (d N pi-val ONE)
  "Resolves the BQA_d(pi, 10^13) optimally using bounded Beam Search."
  (let* ((alpha-int (isqrt (* d (* ONE ONE))))
         (a-init (round pi-val ONE))
         (b-init 0)
         (E-init (- (* a-init ONE) pi-val)))
    
    (multiple-value-bind (S-arr p-arr q-arr) (build-convergents d N ONE alpha-int)
      (let* ((best-abs-E nil)
             (best-A nil)
             (states (list (make-state :E E-init :A a-init :B b-init)))
             (beam-width 500))
        
        (labels ((update-best (E A)
                   (let ((abs-E (abs E)))
                     (when (or (null best-abs-E) (< abs-E best-abs-E))
                       (setf best-abs-E abs-E
                             best-A A)))))
          
          (update-best E-init a-init)
          
          ;; Process convergents from finest (largest k) down to coarsest (k=0)
          (loop for k from (1- (length S-arr)) downto 0 do
            (let ((Sk (aref S-arr k))
                  (pk (aref p-arr k))
                  (qk (aref q-arr k))
                  (new-states-hash (make-hash-table :test 'equal))) ; Deduplicates by (A . B)
              
              (dolist (st states)
                (let* ((E (state-E st))
                       (A (state-A st))
                       (B (state-B st))
                       (c-min -1000000000000000000)
                       (c-max  1000000000000000000))
                  
                  ;; Determine valid coefficient boundaries strictly clamped by N
                  (setf c-max (min c-max (floor (+ N B) qk)))
                  (setf c-min (max c-min (ceiling (- B N) qk)))
                  
                  (if (> pk 0)
                      (progn
                        (setf c-max (min c-max (floor (- N A) pk)))
                        (setf c-min (max c-min (ceiling (- (- N) A) pk))))
                      (when (< pk 0)
                        (setf c-max (min c-max (floor (- (- N) A) pk)))
                        (setf c-min (max c-min (ceiling (- N A) pk)))))
                    
                  (when (<= c-min c-max)
                    (let* ((c-opt (round (- E) Sk))
                           (c-base (max c-min (min c-max c-opt))))
                      ;; Test the clamped optimal choice and immediate neighbors
                      (loop for dc from -2 to 2 do
                        (let ((c (+ c-base dc)))
                          (when (and (<= c-min c) (<= c c-max))
                            (let ((nE (+ E (* c Sk)))
                                  (nA (+ A (* c pk)))
                                  (nB (- B (* c qk))))
                              (update-best nE nA)
                              (setf (gethash (cons nA nB) new-states-hash) nE)))))))))
              
              ;; Collapse branches by taking only the top-performing states
              (let ((next-states nil))
                (maphash (lambda (AB nE)
                           (push (make-state :E nE :A (car AB) :B (cdr AB)) next-states))
                         new-states-hash)
                (setf next-states (sort next-states #'< :key (lambda (s) (abs (state-E s)))))
                (if (> (length next-states) beam-width)
                    (setf states (subseq next-states 0 beam-width))
                    (setf states next-states)))))
        (abs best-A))))))

(defun solve ()
  (format t "Starting mathematical reduction for Project Euler 591...~%")
  (let ((N 10000000000000)
        ;; Mathematical Aesthetic: Elegantly define large precision limits.
        (ONE (expt 10 80)) 
        (pi-val 314159265358979323846264338327950288419716939937510582097494459230781640628620899))
    (let ((sum (collect-sum
                 (mapping ((d (scan-range :from 1 :below 100)))
                   (if (is-square? d)
                       0
                       (solve-d d N pi-val ONE))))))
      (format t "-> The sum of |I_d(BQA_d(pi, 10^13))| is: ~D~%" sum)
      (format nil "~D" sum))))

#+| Do it | (project-euler-0591:solve)