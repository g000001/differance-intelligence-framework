;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0397 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0397)

(defmacro max-fix (a b)
  (let ((va (gensym)) (vb (gensym)))
    `(let ((,va ,a) (,vb ,b))
       (declare (type fixnum ,va ,vb))
       (if (> ,va ,vb) ,va ,vb))))

(defmacro min-fix (a b)
  (let ((va (gensym)) (vb (gensym)))
    `(let ((,va ,a) (,vb ,b))
       (declare (type fixnum ,va ,vb))
       (if (< ,va ,vb) ,va ,vb))))

(defmacro max-fix-3 (a b c)
  (let ((va (gensym)) (vb (gensym)) (vc (gensym)))
    `(let ((,va ,a) (,vb ,b) (,vc ,c))
       (declare (type fixnum ,va ,vb ,vc))
       (let ((m1 (if (> ,va ,vb) ,va ,vb)))
         (declare (type fixnum m1))
         (if (> m1 ,vc) m1 ,vc)))))

(defmacro min-fix-3 (a b c)
  (let ((va (gensym)) (vb (gensym)) (vc (gensym)))
    `(let ((,va ,a) (,vb ,b) (,vc ,c))
       (declare (type fixnum ,va ,vb ,vc))
       (let ((m1 (if (< ,va ,vb) ,va ,vb)))
         (declare (type fixnum m1))
         (if (< m1 ,vc) m1 ,vc)))))

(defparameter *spf* (make-array 1000001 :element-type 'fixnum))

(defun init-spf (limit)
  (declare (type fixnum limit))
  (iterate (for i from 2 to limit)
    (setf (aref *spf* i) i))
  (let ((sqrt-limit (isqrt limit)))
    (iterate (for i from 2 to sqrt-limit)
      (when (= (aref *spf* i) i)
        (iterate (for j from (* i i) to limit by i)
          (when (= (aref *spf* j) j)
            (setf (aref *spf* j) i)))))))

(declaim (inline count-parity))
(defun count-parity (L R P)
  "Counts numbers in [L, R] that are congruent to P mod 2."
  (declare (type fixnum L R P))
  (if (> L R) 0
      (let ((L-adj (if (= (logand L 1) P) L (1+ L)))
            (R-adj (if (= (logand R 1) P) R (1- R))))
        (declare (type fixnum L-adj R-adj))
        (if (> L-adj R-adj) 0
            (1+ (ash (- R-adj L-adj) -1))))))

(defun solve (&optional (k-max 1000000) (x-max 1000000000))
  (declare (type fixnum k-max x-max))
  (init-spf k-max)
  (let ((ans 0)
        (primes (make-array 20 :element-type 'fixnum))
        (counts (make-array 20 :element-type 'fixnum))
        (x-times-2 (* 2 x-max)))
    (declare (type integer ans)
             (type fixnum x-times-2))
    
    (format t "Calculating F(~D, ~D) with Flawless Bounding Boxes...~%" k-max x-max)
    
    (iterate (for k from 1 to k-max)
      (let ((num-factors 0)
            (temp-k k)
            (k2 (* k k)))
        (declare (type fixnum num-factors temp-k k2))
        
        ;; Prime factorization of k
        (iterate (while (> temp-k 1))
          (let ((p (aref *spf* temp-k))
                (c 0))
            (declare (type fixnum p c))
            (iterate (while (= (aref *spf* temp-k) p))
              (incf c)
              (setf temp-k (truncate temp-k p)))
            (setf (aref primes num-factors) p)
            (setf (aref counts num-factors) (* 2 c))
            (incf num-factors)))
            
        ;; Add factor 2 for 2k^2
        (let ((found nil))
          (iterate (for i from 0 below num-factors)
            (when (= (aref primes i) 2)
              (incf (aref counts i))
              (setf found t)
              (return)))
          (unless found
            (setf (aref primes num-factors) 2)
            (setf (aref counts num-factors) 1)
            (incf num-factors)))

        (let ((k2-times-2 (* 2 k2))
              (k2-neg (- k2)))
          (declare (type fixnum k2-times-2 k2-neg))
          
          (labels ((dfs (idx current-div)
                     (declare (type fixnum idx current-div))
                     (if (= idx num-factors)
                         (let* ((D current-div)
                                (E (truncate k2-times-2 D)))
                           (declare (type fixnum D E))
                           
                           ;; Case A: Angle A = 45 -> (k-u)(w+k) = 2k^2
                           (let* ((u (- k D))
                                  (w (- E k))
                                  ;; Flawless Bounds for v
                                  (vmin (max-fix-3 (1+ w) (- (+ u w) x-times-2) (- w u x-times-2)))
                                  (vmax (min-fix (+ u w x-times-2) (+ u (- w) x-times-2)))
                                  (P (logand (+ u w) 1)))
                             (declare (type fixnum u w vmin vmax P))
                             (incf ans (count-parity vmin vmax P))
                             
                             ;; Overlap C=90: vw = -k^2
                             (when (and (not (zerop w)) (zerop (rem k2 w)))
                               (let ((viso1 (truncate k2-neg w)))
                                 (declare (type fixnum viso1))
                                 (when (and (>= viso1 vmin) (<= viso1 vmax) (= (logand viso1 1) P))
                                   (decf ans))))
                                   
                             ;; Overlap B=90: uv = -k^2
                             (when (and (not (zerop u)) (zerop (rem k2 u)))
                               (let ((viso2 (truncate k2-neg u)))
                                 (declare (type fixnum viso2))
                                 (when (and (>= viso2 vmin) (<= viso2 vmax) (= (logand viso2 1) P))
                                   (decf ans)))))
                           
                           ;; Case B: Angle B = 45 -> (u+k)(k-v) = 2k^2
                           (let* ((u (- (- D) k))
                                  (v (+ k E))
                                  ;; Flawless Bounds for w
                                  (wmin (max-fix-3 (1+ u) (- v u x-times-2) (- (+ u v) x-times-2)))
                                  (wmax (min-fix-3 (1- v) (+ u v x-times-2) (+ u (- v) x-times-2)))
                                  (P (logand (+ u v) 1)))
                             (declare (type fixnum u v wmin wmax P))
                             (incf ans (count-parity wmin wmax P))
                             
                             ;; Overlap A=90: uw = -k^2
                             (when (and (not (zerop u)) (zerop (rem k2 u)))
                               (let ((wiso (truncate k2-neg u)))
                                 (declare (type fixnum wiso))
                                 (when (and (>= wiso wmin) (<= wiso wmax) (= (logand wiso 1) P))
                                   (decf ans)))))
                           
                           ;; Case C: Angle C = 45 -> (v+k)(k-w) = 2k^2
                           (let* ((w (- k D))
                                  (v (- E k))
                                  ;; Flawless Bounds for u
                                  (umin (max-fix (- v w x-times-2) (- (+ v w) x-times-2)))
                                  (umax (min-fix-3 (1- w) (+ w (- v) x-times-2) (+ v w x-times-2)))
                                  (P (logand (+ v w) 1)))
                             (declare (type fixnum w v umin umax P))
                             (incf ans (count-parity umin umax P))))
                           
                         (let ((p (aref primes idx))
                               (c (aref counts idx)))
                           (declare (type fixnum p c))
                           (iterate (for i from 0 to c)
                             (dfs (1+ idx) current-div)
                             (when (< i c)
                               (setf current-div (* current-div p))))))))
            
            (dfs 0 1)))
            
        (when (zerop (mod k 100000))
          (format t "Processed k = ~D / ~D, current ans = ~D~%" k k-max ans))))
          
    (format t "Final ans = ~D~%" ans)
    ans))

#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating F(1000000, 1000000000) with Flawless Bounding Boxes...
Processed k = 100000 / 1000000, current ans = 13388504309158360
Processed k = 200000 / 1000000, current ans = 27782206601985050
Processed k = 300000 / 1000000, current ans = 42232154350601334
Processed k = 400000 / 1000000, current ans = 56643381655500074
Processed k = 500000 / 1000000, current ans = 70993253097789728
Processed k = 600000 / 1000000, current ans = 85268579036510604
Processed k = 700000 / 1000000, current ans = 99469327481539252
Processed k = 800000 / 1000000, current ans = 113595878309056704
Processed k = 900000 / 1000000, current ans = 127649527013344384
Processed k = 1000000 / 1000000, current ans = 141630459461893728
Final ans = 141630459461893728

User time    =       59.354
System time  =        0.545
Elapsed time =  0:01:08.588
Allocation   = 306456 bytes
626 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 141630459461893728
:ok