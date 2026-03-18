;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0685 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0685)

(defparameter *mod* 1000000007)

(defun power-mod (base exp)
  (let ((res 1)
        (b (mod base *mod*))
        (e exp))
    (iterate (while (> e 0))
      (when (oddp e)
        (setq res (mod (* res b) *mod*)))
      (setq b (mod (* b b) *mod*))
      (setq e (ash e -1)))
    res))

(defun binom (n k)
  (cond
    ((< k 0) 0)
    ((< n k) 0)
    ((= k 0) 1)
    (t
     (let ((res 1))
       (iterate (for i from 0 below k)
         (setq res (/ (* res (- n i)) (1+ i))))
       res))))

(defun W (l k)
  "Calculates the number of ways to distribute sum K over L digits using inclusion-exclusion."
  (cond
    ((< k 0) 0)
    ((= l 0) (if (= k 0) 1 0))
    (t
     (let ((ans 0))
       (iterate (for j from 0 to (floor k 10))
         (let* ((k-part (- k (* 10 j)))
                (n-part (1- (+ l k-part)))
                (term (* (binom l j) (binom n-part k-part))))
           (if (evenp j)
               (incf ans term)
               (decf ans term))))
       ans))))

(defun find-L-prime (L R target)
  "Binary search to jump over trailing 9s (where y=0) in O(log L) time."
  (let ((low 0)
        (high L)
        (ans L))
    (iterate (while (<= low high))
      (let ((mid (floor (+ low high) 2)))
        (if (>= (W mid R) target)
            (progn
              (setq ans mid)
              (setq high (1- mid)))
            (setq low (1+ mid)))))
    ans))

(defun solve ()
  (let ((ans 0))
    (format t "Starting the search...~%")
    (iterate (for n from 1 to 10000)
      (when (zerop (mod n 1000))
        (format t "Processing n = ~A...~%" n))
      
      (let* ((S-val (expt n 3))
             (m-val (expt n 4))
             (D (ceiling S-val 9))
             (R (- (* 9 D) S-val)))
        
        ;; 1. Determine the correct number of digits D and offset R
        (iterate
          (let ((total (- (W D R) (W (1- D) (- R 9)))))
            (if (<= m-val total)
                (leave)
                (progn
                  (decf m-val total)
                  (incf D)
                  (incf R 9)))))
        
        (let ((V-ans 0))
          ;; 2. Determine the most significant digit (y1 in [0, 8])
          (iterate (for v from (min R 8) downto 0)
            (let ((ways (W (1- D) (- R v))))
              (if (<= m-val ways)
                  (progn
                    (setq V-ans (mod (+ V-ans (* (- 9 v) (power-mod 10 (1- D)))) *mod*))
                    (decf R v)
                    (decf D)
                    (leave))
                  (decf m-val ways))))
          
          ;; 3. Determine remaining digits with O(1) telescoping jumps
          (iterate (while (> R 0))
            (let* ((W-diff (- (W D R) m-val))
                   (L-prime (find-L-prime D R (1+ W-diff))))
              ;; Accrue the block of 9s we jumped over
              (setq V-ans (mod (+ V-ans 
                                  (power-mod 10 D) 
                                  (- *mod* (power-mod 10 L-prime))) 
                               *mod*))
              (setq m-val (- m-val (- (W D R) (W L-prime R))))
              (setq D L-prime)
              
              (let ((found nil))
                (iterate (for v from (min R 9) downto 1)
                  (let ((ways (W (1- D) (- R v))))
                    (if (<= m-val ways)
                        (progn
                          (setq V-ans (mod (+ V-ans (* (- 9 v) (power-mod 10 (1- D)))) *mod*))
                          (decf R v)
                          (decf D)
                          (setq found t)
                          (leave))
                        (decf m-val ways))))
                (unless found
                  (error "State Error: Cannot satisfy combinations.")))))
          
          ;; 4. Accrue any remaining trailing 9s
          (when (> D 0)
            (setq V-ans (mod (+ V-ans (power-mod 10 D) (- *mod* 1)) *mod*)))
          
          (setq ans (mod (+ ans V-ans) *mod*)))))
    (format t "Done.~%")
    (format nil "~A" ans)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting the search...
Processing n = 1000...
Processing n = 2000...
Processing n = 3000...
Processing n = 4000...
Processing n = 5000...
Processing n = 6000...
Processing n = 7000...
Processing n = 8000...
Processing n = 9000...
Processing n = 10000...
Done.

User time    =        0.414
System time  =        0.016
Elapsed time =        0.343
Allocation   = 19942040 bytes
384 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "662878999"
:ok