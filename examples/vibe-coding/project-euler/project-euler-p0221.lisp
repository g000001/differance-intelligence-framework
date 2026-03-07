;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0221 (:use cl iterate alexandria))
(in-package #:project-euler-0221)


#||
(cl-text euler-p221-acx
  (cl-comment "Ontology for Project Euler P221: Alexandrian Integers")
  (cl-comment "Applying Two-Truths Entanglement (二諦随伴) to avoid NMF")

  (forall (A p q r)
    (iff (AlexandrianInteger A)
         (and (PositiveInteger A)
              (Integer p) (Integer q) (Integer r)
              (= A (* p q r))
              (= (/ 1 A) (+ (/ 1 p) (/ 1 q) (/ 1 r))))))

  (cl-comment "Mathematical Reduction (勝義諦への跳躍 ρ)")
  (cl-comment "O(N) search space is logically compressed to divisor search of u^2+1")
  (forall (A p u v x)
    (iff (ReducedAlexandrian A p u v x)
         (and (= q (- u))
              (= r (- v))
              (PositiveInteger p) (PositiveInteger u) (PositiveInteger v)
              (<= p u) (<= u v)
              (= x (- u p))
              (Divides x (+ (* u u) 1))
              (<= x (floor u 2))
              (= p (- u x))
              (= v (- (/ (+ (* u u) 1) x) u))
              (= A (* p u v)))))

  (cl-comment "Bijective Generation (一意生成の厳密化)")
  (cl-comment "Prevents overcounting and completely isolates the strict fundamental domain")
  (forall (u x)
    (if (and (PositiveInteger u)
             (Divides x (+ (* u u) 1))
             (<= x (floor u 2)))
        (GeneratesUniqueAlexandrian u x)))
)
||#


(defmacro with-acx-jump-search ((candidates-var u-var initial-limit &key target-index) &body search-logic)
  "NMFに陥ることを防ぐため、探索空間を動的に拡張しながら再計算可能な跳躍（Restart）構造を構築する"
  (with-gensyms (limit current-u safe-max-a unique-count last-val ans val)
    `(let ((,candidates-var (make-array (* ,target-index 2) 
                                        :fill-pointer 0 
                                        :adjustable t 
                                        :element-type 'integer)))
       (iterate (with ,limit = ,initial-limit)
         (with ,current-u = 1)
         (initially 
          (format t ";; Initiating ACX Jump search up to U=~A~%" ,limit))
         (progn
           (iterate (for ,u-var from ,current-u to ,limit)
             ,@search-logic)
           (setf ,current-u (1+ ,limit)))
                
         ;; 状態の負債の清算（Debt Clearance）: ソートと重複の排除
         (setf ,candidates-var (sort ,candidates-var #'<))
         (let ((,unique-count 0)
               (,last-val -1)
               (,ans nil))
           (iterate (for ,val in-vector ,candidates-var)
             (when (> ,val ,last-val)
               (incf ,unique-count)
               (setf ,last-val ,val)
               (when (= ,unique-count ,target-index)
                 (setf ,ans ,val)
                 (finish))))
                  
           ;; 境界値での安全領域検証
           ;; 与えられたuの限界値において、漏れなく生成できるAの最小理論値
           (let ((,safe-max-a (ash (expt ,limit 3) -1)))
             (if (and ,ans (< ,ans ,safe-max-a))
                 (return ,ans)
                 (progn
                   (setf ,limit (floor (* ,limit 1.25)))
                   (format t ";; NMF risk detected. Restarting (ACX Jump) to limit=~A~%" ,limit)))))))))

(defun verify-base-cases ()
  "問題文で与えられた小さな具体例を用いて内部で論理的トレースを行い、矛盾(不全)がないことを確認"
  (let ((ans (with-acx-jump-search (candidates u 20 :target-index 6)
               (let* ((u2+1 (1+ (* u u)))
                      (x-limit (ash u -1)))
                 (iterate (for x from 1 to x-limit)
                   (when (zerop (rem u2+1 x))
                     (let* ((p (- u x))
                            (v (- (truncate u2+1 x) u)))
                       (vector-push-extend (* p u v) candidates))))))))
    (assert (= ans 630))
    (format t ";; Base cases verified successfully. T(6) = 630.~%")))

(defun solve ()
  (verify-base-cases)
  (let ((ans (with-acx-jump-search (candidates u 150000 :target-index 150000)
               (declare (type fixnum u))
               (let* ((u2+1 (1+ (* u u)))
                      (x-limit (ash u -1)))
                 (declare (type fixnum u2+1 x-limit))
                 ;; 最適化: uの偶奇によって無駄な走査をスキップし、剰余計算の負荷を軽減
                 (if (evenp u)
                     (iterate (for x from 1 to x-limit by 2)
                       (declare (type fixnum x))
                       (when (zerop (rem u2+1 x))
                         (let* ((p (the fixnum (- u x)))
                                (v (the fixnum (- (truncate u2+1 x) u))))
                           (vector-push-extend (* p u v) candidates))))
                     (iterate (for x from 1 to x-limit)
                       (declare (type fixnum x))
                       (when (zerop (rem u2+1 x))
                         (let* ((p (the fixnum (- u x)))
                                (v (the fixnum (- (truncate u2+1 x) u))))
                           (vector-push-extend (* p u v) candidates)))))))))
    (format t "The 150000th Alexandrian integer is: ~A~%" ans)
    ans))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
;; Initiating ACX Jump search up to U=20
;; Base cases verified successfully. T(6) = 630.
;; Initiating ACX Jump search up to U=150000
;; NMF risk detected. Restarting (ACX Jump) to limit=187500
The 150000th Alexandrian integer is: 1884161251122450

User time    =       32.429
System time  =        0.227
Elapsed time =       33.074
Allocation   = 28318512 bytes
6978 Page faults
GC time      =        0.009
 |------------------------------------------------------------|#
;;→ 1884161251122450
:ok
