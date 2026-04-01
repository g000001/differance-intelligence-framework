;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0933 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0933)
(declaim (optimize (speed 3) (safety 0) (debug 0)))


(defconstant $stride 60000)
(defconstant $limit-w 123)
(defconstant $limit-h 1234567)
(defconstant $stable-req 3000)

(defun make-fixnum-array (size)
  (make-array size :element-type 'fixnum :initial-element 0))

(defun solve ()
  (let ((g-table (make-fixnum-array (* 125 $stride)))
        (r-table (make-fixnum-array (* 125 $stride)))
        (seen (make-fixnum-array 4096))
        (total-d 0))
    
    (prog ((w 2) (h 2) (x 1) (y 1) (half-w 0) (half-h 0)
           (nx 0) (ny 0) (s 0) (mex 0) (c-count 0) (c-prev 0)
           (stable-count 0) (w-sum 0) (seen-id 0)
           (ro 0) (o1 0) (o2 0) (rem-h 0) (extrap-sum 0)
           (stable-val 0))
     
loop-w
          (when (> w $limit-w) (go end))
      
          (setf half-w (floor w 2))
      
          ;; R(w, x, y) = G(x, y) XOR G(w-x, y) を全 y について事前計算し、内側ループの演算を極限まで削る
          (setf x 1)
loop-pre-x
          (if (> x half-w) (go start-dp))
          (setf o1 (* x $stride))
          (setf o2 (* (- w x) $stride))
          (setf ro (* x $stride))
          (setf y 1)
loop-pre-y
          (when (>= y $stride)
            (incf x)
            (go loop-pre-x))
          (setf (aref r-table (+ ro y))
                (logxor (aref g-table (+ o1 y))
                        (aref g-table (+ o2 y))))
          (incf y)
          (go loop-pre-y)
      
start-dp
          (setf h 2)
          (setf c-prev 0)
          (setf stable-count 0)
          (setf w-sum 0)
      
loop-h
          (when (> h $limit-h) (go extrap))
          (when (>= h $stride) (error "Dynamic stride exceeded. Increase $stride."))
      
          (setf half-h (floor h 2))
          (setf c-count 0)
          (incf seen-id)
      
          (setf x 1)
loop-x
          (when (> x half-w) (go calc-mex))
          (setf nx (if (= x (- w x)) 1 2))
          (setf ro (* x $stride))
      
          (setf y 1)
loop-y
          (when (> y half-h)
            (incf x)
            (go loop-x))
          (setf ny (if (= y (- h y)) 1 2))
      
          ;; 汚染のない純粋な O(1) ルックアップ (if 分岐なし)
          (setf s (logxor (aref r-table (+ ro y))
                          (aref r-table (+ ro (- h y)))))
      
          (when (= s 0) (incf c-count (* nx ny)))
          (when (< s 4096) (setf (aref seen s) seen-id))
      
          (incf y)
          (go loop-y)
      
calc-mex
          (setf mex 0)
loop-mex
          (when (= (aref seen mex) seen-id)
            (incf mex)
            (go loop-mex))
      
          (setf (aref g-table (+ (* w $stride) h)) mex)
          (incf w-sum c-count)
      
          ;; 不変量の監視: ΔC = w-1
          (if (= (- c-count c-prev) (- w 1))
              (incf stable-count)
              (setf stable-count 0))
          (setf c-prev c-count)
      
          ;; 3000連続の安定で「偽のプラトー」を完全に排除
          (when (>= stable-count $stable-req) (go fill-stable))
      
          (incf h)
          (go loop-h)
      
fill-stable
          ;; 未来の w のために、安定した Grundy 数を stride の果てまで物理的に書き込む (クランプの排除)
          (setf stable-val (aref g-table (+ (* w $stride) h)))
          (setf y (1+ h))
loop-fill
          (when (>= y $stride) (go extrap))
          (setf (aref g-table (+ (* w $stride) y)) stable-val)
          (incf y)
          (go loop-fill)
      
extrap
          (setf rem-h (- $limit-h h))
          (when (> rem-h 0)
            ;; O(1) 等差数列の和による外挿
            (setf extrap-sum (+ (* rem-h c-count)
                                (floor (* (- w 1) rem-h (1+ rem-h)) 2)))
            (incf w-sum extrap-sum))
      
          (when (= (mod w 10) 0)
            (format t "Debug: w=~A, true stability at h=~A, w-sum=~A~%" w (- h $stable-req) w-sum))
      
          (incf total-d w-sum)
          (incf w)
          (go loop-w)
      
end
          )
    
    (format t "D(~A, ~A) = ~A~%" $limit-w $limit-h total-d)
    (return-from solve total-d)))

#+| Do it | (project-euler-0933:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Debug: w=10, dynamically stabilized at h=91, w-sum=6858314755541
Debug: w=20, dynamically stabilized at h=317, w-sum=14476257559283
Debug: w=30, dynamically stabilized at h=639, w-sum=22090132404953
Debug: w=40, dynamically stabilized at h=1143, w-sum=29697550754739
Debug: w=50, dynamically stabilized at h=1323, w-sum=37302265357061
Debug: w=60, dynamically stabilized at h=1365, w-sum=44902850201775
Debug: w=70, dynamically stabilized at h=1491, w-sum=52502197441393
Debug: w=80, dynamically stabilized at h=1771, w-sum=60100465327815
Debug: w=90, dynamically stabilized at h=2324, w-sum=67694845827669
Debug: w=100, dynamically stabilized at h=3656, w-sum=75283410620979
Debug: w=110, dynamically stabilized at h=4813, w-sum=82860112531469
Debug: w=120, dynamically stabilized at h=4982, w-sum=90438161090099
D(123, 1234567) = 5707485980741639

User time    =  0:12:25.529
System time  =       17.740
Elapsed time =  0:18:40.827
Allocation   = 215303432 bytes
57370 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 5707485980741639
:ok