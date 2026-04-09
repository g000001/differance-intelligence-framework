;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0456 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0456)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)



(defun choose2 (n)
  (if (>= n 2) (ash (* n (1- n)) -1) 0))

(defun choose3 (n)
  (if (>= n 3) (/ (* n (* (1- n) (- n 2))) 6) 0))

(defun solve (&optional (n 2000000))
  (let* ((xs (make-array n :element-type 'fixnum))
         (ys (make-array n :element-type 'fixnum))
         (indices (make-array n :element-type 'fixnum))
         (x-pow 1)
         (y-pow 1))
    (declare (type fixnum x-pow y-pow))
    
    ;; 点の生成
    (iterate (for i from 0 below n)
      (setf x-pow (mod (* x-pow 1248) 32323))
      (setf y-pow (mod (* y-pow 8421) 30103))
      (setf (aref xs i) (- x-pow 16161))
      (setf (aref ys i) (- y-pow 15051))
      (setf (aref indices i) i))
    
    (labels ((half-plane (x y)
               (declare (type fixnum x y))
               ;; 0: 上半平面および正のX軸, 1: 下半平面および負のX軸
               (if (or (> y 0) (and (= y 0) (> x 0))) 0 1))
             
             (cmp (i j)
               (let* ((x1 (aref xs i)) (y1 (aref ys i))
                      (x2 (aref xs j)) (y2 (aref ys j))
                      (h1 (half-plane x1 y1))
                      (h2 (half-plane x2 y2)))
                 (declare (type fixnum x1 y1 x2 y2 h1 h2))
                 (if (/= h1 h2)
                     (< h1 h2)
                     (let ((cross (- (* x1 y2) (* x2 y1))))
                       (declare (type fixnum cross))
                       (> cross 0))))))
      
      ;; 角度によるソート
      (sort indices #'cmp)
      
      (let ((gx (make-array (* 2 n) :element-type 'fixnum))
            (gy (make-array (* 2 n) :element-type 'fixnum))
            (gw (make-array (* 2 n) :element-type 'fixnum))
            (k 0)
            (w-0 0)
            (n-prime 0))
        (declare (type fixnum k w-0 n-prime))
        
        ;; 同一の偏角を持つ点のグループ化
        (iterate (for idx in-vector indices)
          (let ((x (aref xs idx))
                (y (aref ys idx)))
            (declare (type fixnum x y))
            (if (and (= x 0) (= y 0))
                (incf w-0)
                (progn
                  (incf n-prime)
                  (if (= k 0)
                      (progn
                        (setf (aref gx k) x
                              (aref gy k) y
                              (aref gw k) 1)
                        (incf k))
                      (let ((last-x (aref gx (1- k)))
                            (last-y (aref gy (1- k))))
                        (declare (type fixnum last-x last-y))
                        (if (and (= (half-plane x y) (half-plane last-x last-y))
                                 (= (- (* x last-y) (* last-x y)) 0))
                            (incf (aref gw (1- k)))
                            (progn
                              (setf (aref gx k) x
                                    (aref gy k) y
                                    (aref gw k) 1)
                              (incf k)))))))))
        
        ;; 円環構造を模倣するため配列を2倍に拡張
        (iterate (for i from 0 below k)
          (setf (aref gx (+ k i)) (aref gx i)
                (aref gy (+ k i)) (aref gy i)
                (aref gw (+ k i)) (aref gw i)))
        
        (let ((j 1)
              (sum-w 0)
              (total-t-out 0)
              (total-t-edge 0))
          (declare (type fixnum j sum-w))
          
          ;; 尺取り法による 180度未満の範囲の探索
          (iterate (for i from 0 below k)
            (when (<= j i)
              (setf j (1+ i))
              (setf sum-w 0))
            
            (iterate
              (while (< j (+ i k)))
              (let ((cross (- (* (aref gx i) (aref gy j))
                              (* (aref gx j) (aref gy i)))))
                (declare (type fixnum cross))
                (if (> cross 0)
                    (progn
                      (incf sum-w (aref gw j))
                      (incf j))
                    (return))))
            
            (let ((w-i (aref gw i))
                  (s-i sum-w))
              (declare (type fixnum w-i s-i))
              ;; 原点を含まない三角形の加算
              (incf total-t-out (+ (choose3 w-i)
                                   (* (choose2 w-i) s-i)
                                   (* w-i (choose2 s-i))))
              
              ;; 角度差がちょうど180度の反対側の点の処理
              (when (< j (+ i k))
                (let ((cross (- (* (aref gx i) (aref gy j))
                                (* (aref gx j) (aref gy i)))))
                  (declare (type fixnum cross))
                  (when (= cross 0)
                    (let ((w-j (aref gw j)))
                      (declare (type fixnum w-j))
                      (incf total-t-edge (+ (* w-i w-j (- n-prime w-i w-j))
                                            (* (choose2 w-i) w-j)
                                            (* w-i (choose2 w-j)))))))))
            
            ;; ウィンドウを進める前のクリーンアップ
            (when (> j (1+ i))
              (decf sum-w (aref gw (1+ i)))))
          
          ;; 反対側ペアは両端から2回カウントされるため補正
          (setf total-t-edge (/ total-t-edge 2))
          
          (let* ((t-all (choose3 n))
                 (t-origin (+ (choose3 w-0)
                              (* (choose2 w-0) (- n w-0))
                              (* w-0 (choose2 (- n w-0)))))
                 (ans (- t-all t-origin total-t-out total-t-edge)))
            
            (when (member n '(8 600 40000 2000000))
              (format t "N=~D: All=~D, Out=~D, Edge=~D, Orig=~D~%"
                      n t-all total-t-out total-t-edge t-origin)
              (format t "C(~D) = ~D~%" n ans))
            ans))))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
N=2000000: All=180409828727153024, Out=999998075662236589, Edge=48985791865, Orig=0
C(2000000) = -819588295920875430

User time    =        1.788
System time  =        0.073
Elapsed time =        1.803
Allocation   = 144650280 bytes
60293 Page faults
GC time      =        0.091
 |------------------------------------------------------------|#
;;→ -819588295920875430
:ok