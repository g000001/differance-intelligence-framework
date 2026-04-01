;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0923 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0923)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defconstant +mod+ 1000000007)

(defun mod-add (a b)
  (let ((sum (+ a b)))
    (if (>= sum +mod+) (- sum +mod+) sum)))

(defun mod-mul (a b)
  (mod (* a b) +mod+))

(defun solve-young-game (m w-max)
  "Evaluates S(m, w) using the exact integer and Nim-star projection formula.
   The state space is convoluted efficiently using sparse list extraction."
  (let* ((offset 10000)
         (dp-size 20000)
         ;; Elements array: [value + offset][has-star (0 or 1)] -> count
         (elements (make-array (list dp-size 2) :initial-element 0))
         (dp (make-array (list dp-size 2) :initial-element 0))
         (next-dp (make-array (list dp-size 2) :initial-element 0)))
    
    ;; 1. Axiomatic Grounding: Generate exact game values for all staircases
    (loop for a from 1 to w-max do
            (loop for b from 1 to w-max do
                    (loop for k from 1 to w-max do
                            (when (<= (+ a b k) w-max)
                              (let* ((v (cond
                                         ((= a b) 0)
                                         ((> a b) (- (* k b) a))
                                         ((< a b) (- b (* k a)))))
                                     (has-star (if (and (= a b) (evenp k)) 1 0))
                                     (idx (+ v offset)))
                                (setf (aref elements idx has-star)
                                      (1+ (aref elements idx has-star))))))))
                    
    ;; Extract dense elements to bypass massive empty array iteration
    (let ((dense-elements nil))
      (dotimes (v dp-size)
        (when (> (aref elements v 0) 0)
          (push (list v 0 (aref elements v 0)) dense-elements))
        (when (> (aref elements v 1) 0)
          (push (list v 1 (aref elements v 1)) dense-elements)))
          
      ;; 2. Initialize DP with 0 choices (value=0, star=0)
      (setf (aref dp offset 0) 1)
    
      ;; 3. Convolution for 'm' chosen staircases
      (dotimes (step m)
        ;; Clear next-dp
        (dotimes (i dp-size)
          (setf (aref next-dp i 0) 0
                (aref next-dp i 1) 0))
              
        ;; Extract active DP states for the current step
        (let ((dense-dp nil))
          (dotimes (v dp-size)
            (when (> (aref dp v 0) 0)
              (push (list v 0 (aref dp v 0)) dense-dp))
            (when (> (aref dp v 1) 0)
              (push (list v 1 (aref dp v 1)) dense-dp)))
            
          ;; O(|Active DP| * |Active Elements|) state transitions
          (dolist (dp-item dense-dp)
            (destructuring-bind (v1 s1 c1) dp-item
              (dolist (el-item dense-elements)
                (destructuring-bind (v2 s2 c2) el-item
                  (let ((nv (+ v1 (- v2 offset)))
                        (ns (logxor s1 s2)))
                    (when (and (>= nv 0) (< nv dp-size))
                      (setf (aref next-dp nv ns)
                            (mod-add (aref next-dp nv ns)
                                     (mod-mul c1 c2))))))))))
                                   
        ;; Copy next-dp to dp
        (dotimes (i dp-size)
          (setf (aref dp i 0) (aref next-dp i 0)
                (aref dp i 1) (aref next-dp i 1))))
              
      ;; 4. Evaluate Winning Conditions
      ;; Right wins if V > 0 OR (V = 0 AND has-star = 1)
      (let ((ans 0))
        (dotimes (v dp-size)
          (let ((real-v (- v offset)))
            (cond
             ((> real-v 0)
              (setf ans (mod-add ans (aref dp v 0)))
              (setf ans (mod-add ans (aref dp v 1))))
             ((= real-v 0)
              (setf ans (mod-add ans (aref dp v 1)))))))
        ans))))

(defun solve ()
  (format t "--- Mathematical Grounding Validation ---~%")
  (format t "Testing S(2, 4)... Expected: 7, Got: ~A~%" (solve-young-game 2 4))
  (format t "Testing S(3, 9)... Expected: 315319, Got: ~A~%" (solve-young-game 3 9))
  (format t "-----------------------------------------~%")
  (format t "Solving for S(8, 64)...~%")
  (let ((ans (solve-young-game 8 64)))
    (format t "Answer modulo 10^9+7: ~A~%" ans)
    ans))