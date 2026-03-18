;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0513 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0513)

(defun solve ()
  (let ((ans 0)
        (n 100000))
    (declare (type fixnum ans n)
             (optimize (speed 3) (safety 0) (debug 0)))
             
    (format t "Starting evaluation (Expected max v is ~A)...~%" (floor n 2))
    
    (let ((v-max (floor n 2)))
      (declare (type fixnum v-max))
      ;; 1st Loop: v
      (iterate (for v from 2 to v-max)
        (declare (type fixnum v))
        
        ;; 進捗出力
        (when (zerop (mod v 10000))
          (format t "Processing v = ~A / ~A~%" v v-max))
          
        ;; 2nd Loop: u (must be strictly less than v for A < C)
        (iterate (for u from 1 to (1- v))
          (declare (type fixnum u))
          (when (= (gcd u v) 1)
            
            (let* ((u-odd (logand u 1))
                   (v-odd (logand v 1))
                   ;; Parity mapping: if u and v have different parities, g MUST be even.
                   (g-step (if (/= u-odd v-odd) 2 1))
                   (g-max (floor n (* 2 v))))
              (declare (type fixnum u-odd v-odd g-step g-max))
              
              ;; 3rd Loop: g
              (iterate (for g from g-step to g-max by g-step)
                (declare (type fixnum g))
                
                (let* ((c-val (* g v))
                       (a-val (* g u))
                       ;; C <= D => w >= C / u
                       (w-min (floor (+ c-val u -1) u))
                       ;; C + D <= n => w <= (n - C) / u
                       (w-max1 (floor (- n c-val) u))
                       ;; A + B <= 3C + D => w <= (3C - A) / (v - u)
                       (w-max2 (floor (- (* 3 c-val) a-val) (- v u)))
                       (w-max (min w-max1 w-max2)))
                  (declare (type fixnum c-val a-val w-min w-max1 w-max2 w-max))
                  
                  ;; O(1) Innermost evaluation:
                  ;; w must have the same parity as C.
                  (when (<= w-min w-max)
                    (let* ((c-mod (logand c-val 1))
                           (w-min-mod (logand w-min 1))
                           (w-max-mod (logand w-max 1))
                           (start (if (= w-min-mod c-mod) w-min (1+ w-min)))
                           (end (if (= w-max-mod c-mod) w-max (1- w-max))))
                      (declare (type fixnum c-mod w-min-mod w-max-mod start end))
                      
                      (when (<= start end)
                        (incf ans (1+ (ash (- end start) -1)))))))))))))
                        
    (format t "Done.~%")
    (format nil "~A" ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting evaluation (Expected max v is 50000)...
Processing v = 10000 / 50000
Processing v = 20000 / 50000
Processing v = 30000 / 50000
Processing v = 40000 / 50000
Processing v = 50000 / 50000
Done.

User time    =  0:03:38.188
System time  =        0.319
Elapsed time =  0:03:37.699
Allocation   = 3355544 bytes
3885 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "2925619196"
:ok
