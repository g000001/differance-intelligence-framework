;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0246 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0246)

#||
(cl-text https://projecteuler.net/problem=246
  (cl-comment "Project Euler 246: Tangents to an ellipse")
  (cl-comment "Let the ellipse be centered at origin: x^2/a^2 + y^2/b^2 = 1")
  (cl-comment "a^2 = 56250000, b^2 = 31250000")
  (cl-comment "A point P(u,v) outside the ellipse has angle > 45 degrees iff")
  (cl-comment "4A > B^2 or B <= 0, where A = b^2u^2 + a^2v^2 - a^2b^2 and B = u^2 + v^2 - a^2 - b^2")
  (forall (u v)
    (iff (AngleGreaterThan45 u v)
         (and (> (A u v) 0)
              (or (<= (B u v) 0)
                  (> (* 4 (A u v)) (Square (B u v)))))))
  )
||#

(defun inside-p (u v)
  "点 P(u, v) が楕円の外部かつ2接線のなす角が45度より大きいか判定する"
  (declare (type integer u v))
  (let* ((a2 56250000)
         (b2 31250000)
         (a2b2 1757812500000000)
         (a2+b2 (+ a2 b2))
         (u2 (* u u))
         (v2 (* v v))
         (A (- (+ (* b2 u2) (* a2 v2)) a2b2))
         (B (- (+ u2 v2) a2+b2)))
    (and (> A 0)
         (or (<= B 0)
             (> (* 4 A) (* B B))))))

(defun find-v-min (u)
  "与えられた u に対して、楕円の外部 (A > 0) となる最小の v >= 0 を二分探索で見つける"
  (declare (type integer u))
  (let ((low 0)
        (high 25000)
        (ans 25001))
    (iterate (while (<= low high))
             (let* ((mid (ash (+ low high) -1))
                    (a2 56250000)
                    (b2 31250000)
                    (a2b2 1757812500000000)
                    (A (- (+ (* b2 (* u u)) (* a2 (* mid mid))) a2b2)))
               (if (> A 0)
                   (progn (setf ans mid)
                          (setf high (1- mid)))
                   (setf low (1+ mid)))))
    ans))

(defun find-v-max (u v-min)
  "与えられた u に対して、条件を満たす最大の v >= 0 を二分探索で見つける"
  (declare (type integer u v-min))
  (let ((low v-min)
        (high 25000)
        (ans (1- v-min)))
    (iterate (while (<= low high))
             (let ((mid (ash (+ low high) -1)))
               (if (inside-p u mid)
                   (progn (setf ans mid)
                          (setf low (1+ mid)))
                   (setf high (1- mid)))))
    ans))

(defun solve ()
  (let ((total 0)
        (max-u 25000))
    (format t "Starting sweep-line binary search algorithm...~%")
    ;; 第1象限 (u >= 0, v >= 0) で条件を満たす区間を探索し、対称性を利用して全体を数え上げる
    (iterate (for u from 0 to max-u)
             (let ((v-min (find-v-min u)))
               (when (<= v-min max-u)
                 (let ((v-max (find-v-max u v-min)))
                   (when (>= v-max v-min)
                     (let ((count (1+ (- v-max v-min))))
                       (if (= u 0)
                           ;; u = 0 のときの対称性ウェイト (v=0なら中心だが外部なのでv-min>0が保証されている)
                           (if (= v-min 0)
                               (incf total (+ 1 (* 2 (1- count))))
                               (incf total (* 2 count)))
                           ;; u > 0 のときの対称性ウェイト
                           (if (= v-min 0)
                               (incf total (+ 2 (* 4 (1- count))))
                               (incf total (* 4 count))))))))))
    (format t "Completed.~%")
    total))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting sweep-line binary search algorithm...
Completed.

User time    =        0.032
System time  =        0.001
Elapsed time =        0.017
Allocation   = 34696 bytes
989 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 810834388
:ok