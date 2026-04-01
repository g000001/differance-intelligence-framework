;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0584 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0584)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun generate-states (max-k w-days)
  (let ((states-list nil))
    (labels ((dfs (depth current-sum acc)
               (if (= depth w-days)
                   (push (reverse acc) states-list)
                   (loop for c from 0 to (- max-k current-sum) do
                     (dfs (1+ depth) (+ current-sum c) (cons c acc))))))
      (dfs 0 0 nil))
    (nreverse states-list)))

(defun valid-wrap-gen? (s-end s-start w-days max-k)
  (let ((arr (make-array (* 2 w-days) :element-type 'fixnum)))
    (loop for i from 0 to (1- w-days) do (setf (aref arr i) (nth i s-end)))
    (loop for i from 0 to (1- w-days) do (setf (aref arr (+ i w-days)) (nth i s-start)))
    (loop for i from 0 to (1- w-days)
          always (<= (loop for j from i to (+ i w-days) sum (aref arr j)) max-k))))

(defun run-dp-fast (days w-days max-k max-n states num-states)
  (let* ((transitions nil)
         (num-tr 0))
    ;; Generate state transitions
    (iterate ((s (scan states))
              (from-idx (scan-range)))
      (let ((sum (loop for x in s sum x)))
        (iterate ((c-new (scan-range :from 0 :upto (- max-k sum))))
          (let* ((s-prime (append (cdr s) (list c-new)))
                 (to-idx (position s-prime states :test #'equal)))
            (push (list from-idx to-idx c-new
                        (if (= c-new 0) 1.0d0
                            (if (= c-new 1) 1.0d0
                                (if (= c-new 2) 0.5d0
                                    (if (= c-new 3) (/ 1.0d0 6.0d0) 0.0d0)))))
                  transitions)))))
    (setq transitions (nreverse transitions))
    (setq num-tr (length transitions))
    
    ;; Flatten transitions for cache-efficient loop
    (let ((tr-from (make-array num-tr :element-type 'fixnum))
          (tr-to (make-array num-tr :element-type 'fixnum))
          (tr-c-new (make-array num-tr :element-type 'fixnum))
          (tr-fact (make-array num-tr :element-type 'double-float)))
      (loop for tr in transitions
            for i from 0
            do (setf (aref tr-from i) (first tr)
                     (aref tr-to i) (second tr)
                     (aref tr-c-new i) (third tr)
                     (aref tr-fact i) (fourth tr)))
                     
      (let* ((size (the fixnum (* num-states (the fixnum (* num-states max-n)))))
             (dp (make-array size :element-type 'double-float :initial-element 0.0d0))
             (next-dp (make-array size :element-type 'double-float :initial-element 0.0d0)))
        
        ;; Initialize DP with start states
        (loop for s in states
              for idx from 0
              do (let ((sum (loop for x in s sum x))
                       (fact-prod 1.0d0))
                   (loop for x in s do
                           (setf fact-prod (* fact-prod (if (= x 2) 2.0d0 (if (= x 3) 6.0d0 1.0d0)))))
                   (let ((offset (the fixnum (+ sum (the fixnum (* max-n (the fixnum (+ idx (the fixnum (* idx num-states))))))))))
                     (setf (aref dp offset) (/ 1.0d0 fact-prod)))))
                     
        ;; DP Evolution
        (dotimes (step (the fixnum (- days w-days)))
          (fill next-dp 0.0d0)
          (dotimes (i num-tr)
            (let ((from (aref tr-from i))
                  (to (aref tr-to i))
                  (c-new (aref tr-c-new i))
                  (fact-inv (aref tr-fact i)))
              (declare (type fixnum from to c-new)
                       (type double-float fact-inv))
              (let ((base-to (the fixnum (* to (the fixnum (* num-states max-n)))))
                    (base-from (the fixnum (* from (the fixnum (* num-states max-n))))))
                (declare (type fixnum base-to base-from))
                (dotimes (s-start num-states)
                  (let ((offset-to (the fixnum (+ base-to (the fixnum (* s-start max-n)))))
                        (offset-from (the fixnum (+ base-from (the fixnum (* s-start max-n))))))
                    (declare (type fixnum offset-to offset-from))
                    (dotimes (n (the fixnum (1+ (the fixnum (- max-n 1 c-new)))))
                      (let ((idx-to (the fixnum (+ offset-to (the fixnum (+ n c-new)))))
                            (idx-from (the fixnum (+ offset-from n))))
                        (incf (aref next-dp idx-to)
                              (* (aref dp idx-from) fact-inv)))))))))
          (let ((temp dp))
            (setf dp next-dp)
            (setf next-dp temp)))
        dp))))

(defun calculate-expected-value (dp days w-days max-k max-n states num-states)
  (let ((poly-c (make-array max-n :element-type 'double-float :initial-element 0.0d0)))
    ;; Apply circular boundary conditions and extract trace-equivalent polynomials
    (dotimes (s-end num-states)
      (dotimes (s-start num-states)
        (when (valid-wrap-gen? (nth s-end states) (nth s-start states) w-days max-k)
          (let ((base-offset (the fixnum (* max-n (the fixnum (+ s-start (the fixnum (* s-end num-states))))))))
            (declare (type fixnum base-offset))
            (dotimes (n max-n)
              (incf (aref poly-c n) (aref dp (the fixnum (+ base-offset n)))))))))
              
    ;; E[X] = sum_{n} C_n * (n! / D^n)
    (let ((expected 0.0d0)
          (fact-over-d-n 1.0d0))
      (dotimes (n max-n)
        (incf expected (* (aref poly-c n) fact-over-d-n))
        (setf fact-over-d-n (* fact-over-d-n (/ (float (1+ n) 0.0d0) (float days 0.0d0)))))
      expected)))

(defun solve-problem (days window-size k-people)
  (let* ((w-days (1- window-size))
         (max-k (1- k-people))
         ;; Max valid population before breaking rule, with safety margin
         (max-n (+ (floor (* days max-k) window-size) w-days 2))
         (states (generate-states max-k w-days))
         (num-states (length states)))
    (let ((dp (run-dp-fast days w-days max-k max-n states num-states)))
      (calculate-expected-value dp days w-days max-k max-n states num-states))))

(defun solve ()
  (format t "Testing WimWi (D=10, k=3, w=1): ~,8F~%" (solve-problem 10 2 3))
  (format t "Testing Joka (D=100, k=3, w=7): ~,8F~%" (solve-problem 100 8 3))
  (format t "Solving for Earth (D=365, k=4, w=7)...~%")
  (let ((ans (solve-problem 365 8 4)))
    (format t "Answer: ~,8F~%" ans)
    ans))


#+| Do it | (solve )