;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0972 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0972)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun make-vec3 (x y z)
  (let ((v (make-array 3 :element-type 'integer)))
    (setf (aref v 0) x)
    (setf (aref v 1) y)
    (setf (aref v 2) z)
    v))

(defun vec3-x (v) (aref v 0))
(defun vec3-y (v) (aref v 1))
(defun vec3-z (v) (aref v 2))

(defun generate-points (n)
  (let ((points (make-array 0 :fill-pointer 0 :adjustable t)))
    (vector-push-extend (cons 0 1) points)
    ;; LoopをSeriesに置換
    (iterate ((q (scan-range :from 2 :upto n)))
      (iterate ((p (scan-range :from 1 :upto (1- q))))
        (when (= (gcd p q) 1)
          (vector-push-extend (cons p q) points)
          (vector-push-extend (cons (- p) q) points))))
          
    (let* ((valid-points (make-array 0 :fill-pointer 0 :adjustable t))
           (len (length points)))
      (iterate ((i (scan-range :from 0 :below len)))
        (iterate ((j (scan-range :from 0 :below len)))
          (let* ((px (aref points i))
                 (py (aref points j))
                 (x-num (car px)) (x-den (cdr px))
                 (y-num (car py)) (y-den (cdr py))
                 (nx (* x-num y-den))
                 (ny (* y-num x-den))
                 (den (* x-den y-den)))
            (when (< (+ (* nx nx) (* ny ny)) (* den den))
              (vector-push-extend (list x-num x-den y-num y-den) valid-points)))))
      valid-points)))

(defun make-vec-for-point (pt)
  (destructuring-bind (xn xd yn yd) pt
    (let* ((lcm-d (lcm xd yd))
           (vx (* xn (/ lcm-d xd)))
           (vy (* yn (/ lcm-d yd)))
           (big-x (* vx lcm-d))
           (big-y (* vy lcm-d))
           (big-z (+ (* vx vx) (* vy vy) (* lcm-d lcm-d)))
           (g (gcd (abs big-x) (gcd (abs big-y) (abs big-z)))))
      (make-vec3 (truncate big-x g) (truncate big-y g) (truncate big-z g)))))

(defun cross-and-pack (v1 v2)
  (let* ((x1 (vec3-x v1)) (y1 (vec3-y v1)) (z1 (vec3-z v1))
         (x2 (vec3-x v2)) (y2 (vec3-y v2)) (z2 (vec3-z v2))
         (nx (- (* y1 z2) (* z1 y2)))
         (ny (- (* z1 x2) (* x1 z2)))
         (nz (- (* x1 y2) (* y1 x2))))
    (if (and (= nx 0) (= ny 0) (= nz 0))
        -1
        (let* ((g (gcd (abs nx) (gcd (abs ny) (abs nz))))
               (rx (truncate nx g))
               (ry (truncate ny g))
               (rz (truncate nz g)))
          ;; 法線ベクトルの方向（符号）の一意化
          (when (or (< rx 0)
                    (and (= rx 0) (< ry 0))
                    (and (= rx 0) (= ry 0) (< rz 0)))
            (setf rx (- rx) ry (- ry) rz (- rz)))
            
          ;; Bignumへの昇格を許容し、余裕を持ったビット幅(21ビット)でパッキング
          (let ((offset 1048576)) ; 2^20
            (+ (+ rx offset)
               (ash (+ ry offset) 21)
               (ash (+ rz offset) 42)))))))

(defun solve-for (n)
  (let* ((pts (generate-points n))
         (v-count (length pts))
         (vecs (make-array v-count)))
    (format t "Generated ~A valid points for N=~A~%" v-count n)
    (iterate ((i (scan-range :from 0 :below v-count)))
      (setf (aref vecs i) (make-vec-for-point (aref pts i))))
      
    (let* ((num-pairs (floor (* v-count (1- v-count)) 2))
           ;; fixnumの限界を超過するため integer 型の配列を採用
           (pairs (make-array num-pairs :element-type 'integer))
           (idx 0))
      (format t "Computing and packing ~A pairs...~%" num-pairs)
      (iterate ((i (scan-range :from 0 :below v-count)))
        (let ((v1 (aref vecs i)))
          (iterate ((j (scan-range :from (1+ i) :below v-count)))
            (setf (aref pairs idx) (cross-and-pack v1 (aref vecs j)))
            (incf idx))))
            
      (format t "Sorting ~A packed signatures...~%" num-pairs)
      (sort pairs #'<)
      
      (format t "Counting run-lengths and extracting triplets...~%")
      (let ((total-triplets 0)
            (current-val (aref pairs 0))
            (count 1))
        (iterate ((i (scan-range :from 1 :below num-pairs)))
          (let ((val (aref pairs i)))
            (if (= val current-val)
                (incf count)
                (progn
                  (when (>= count 3)
                    (let ((k (floor (+ 1 (isqrt (+ 1 (* 8 count)))) 2)))
                      (incf total-triplets (* k (1- k) (- k 2)))))
                  (setf current-val val)
                  (setf count 1)))))
        ;; 最後のランレングスの処理
        (when (>= count 3)
          (let ((k (floor (+ 1 (isqrt (+ 1 (* 8 count)))) 2)))
            (incf total-triplets (* k (1- k) (- k 2)))))
        total-triplets))))

(defun solve ()
  (format t "Verifying T(2) = ~A (Expected: 24)~%" (solve-for 2))
  (format t "Verifying T(3) = ~A (Expected: 1296)~%" (solve-for 3))
  (let ((ans (solve-for 12)))
    (format t "Final Answer T(12) = ~A~%" ans)
    ans))

#+| Do it | (project-euler-0972:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Generated 9 valid points for N=2
Computing and packing 36 pairs...
Sorting 36 packed signatures...
Counting run-lengths and extracting triplets...
Verifying T(2) = 24 (Expected: 24)
Generated 49 valid points for N=3
Computing and packing 1176 pairs...
Sorting 1176 packed signatures...
Counting run-lengths and extracting triplets...
Verifying T(3) = 1296 (Expected: 1296)
Generated 6837 valid points for N=12
Computing and packing 23368866 pairs...
Sorting 23368866 packed signatures...
Counting run-lengths and extracting triplets...
Final Answer T(12) = 3575508

User time    =       36.163
System time  =        0.605
Elapsed time =       36.772
Allocation   = 951236768 bytes
318681 Page faults
GC time      =        3.321
 |------------------------------------------------------------|#
;;→ 3575508
:ok