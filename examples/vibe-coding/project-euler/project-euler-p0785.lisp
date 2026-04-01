;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0785 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0785)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defun solve-euler-0785 (&optional (limit 1000000000))
  "Solves PE 785 using exact integer projective parameterization over Z[sqrt(-3)].
   Utilizes Congruence-Jumping to completely bypass the O(N) Red Line iteration barrier."
  (let ((ans 0)
        (visited (make-hash-table :test 'eql))
        (max-x (* 76 limit)))
    
    (do ((v 1 (1+ v)))
        (nil)
      (when (zerop (mod v 10000))
        (format t "Progress: v = ~A~%" v))
        
      (let* ((u-start (1+ (floor (* 7 v) 3)))
             (x-start (* (+ (* 5 u-start) v) (+ u-start v))))
        ;; Absolute safety stop based on the minimal bounds of the sector
        (when (> x-start max-x)
          (return ans))
          
        (do ((u u-start))
            (nil)
          (let ((x (* (+ (* 5 u) v) (+ u v))))
            (when (> x max-x)
              (return))
              
            (let ((max-possible-g 1))
              (when (and (oddp u) (oddp v))
                (setf max-possible-g 4))
              (when (= (mod (+ u (* 4 v)) 19) 0)
                (setf max-possible-g (* max-possible-g 19)))
                
              (cond
                ;; Case 1: Within the bounds of the theoretical max possible gcd
                ((<= x (* max-possible-g limit))
                 (when (= (gcd u v) 1)
                   (let* ((y (* (- (* 3 u) (* 7 v)) (- u v)))
                          (u+4v (+ u (* 4 v)))
                          (z (* 4 v u+4v))
                          (g (gcd x (gcd y z)))
                          (nx (/ x g)))
                     ;; x is mathematically proven to be the absolute maximum in this sector.
                     (when (<= nx limit)
                       (let* ((ny (/ y g))
                              (nz (/ z g))
                              (mn (min ny nz))
                              (md (max ny nz))
                              ;; Pack elements securely avoiding heap consing
                              (key (logior (ash mn 30) md)))
                         (unless (gethash key visited)
                           (setf (gethash key visited) t)
                           (incf ans (+ nx ny nz)))))))
                 (incf u 1))
                 
                ;; Case 2: Out of bounds for current g. Needs g=76 to survive.
                ;; Jumps u dynamically to the next valid mod 38 position.
                ((> x (* 19 limit))
                 (if (evenp v)
                     (return) ;; Cannot reach g=76, end loop for this v
                     (let* ((target (mod (* -4 v) 19))
                            (target-odd (if (evenp target) (+ target 19) target))
                            (rem (mod u 38))
                            (step (- target-odd rem)))
                       (when (<= step 0) (incf step 38))
                       (incf u step))))
                       
                ;; Case 3: Needs g >= 19. Jumps u dynamically to the next mod 19 position.
                ((or (> x (* 4 limit)) (and (> x limit) (evenp v)))
                 (let* ((target (mod (* -4 v) 19))
                        (rem (mod u 19))
                        (step (- target rem)))
                   (when (<= step 0) (incf step 19))
                   (incf u step)))
                   
                ;; Case 4: Needs g >= 4, step gently.
                (t
                 (incf u 1))))))))
    ans))

(defun solve ()
  (format t "--- Mathematical Grounding Validation ---~%")
  (format t "Testing S(10^2)... Expected: 184, Got: ~A~%" (solve-euler-0785 100))
  (format t "-----------------------------------------~%")
  (format t "Solving for S(10^9)...~%")
  (let ((ans (solve-euler-0785 1000000000)))
    (format t "Answer: ~A~%" ans)
    ans))