;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0905 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0905)

(defun next-turn (val p)
  "Returns the smallest t > val such that (t - 1) % 3 == p"
  (prog ((t-val (1+ val))
         (rem-val 0)
         (diff 0))
        (setf (values rem-val) (mod (1- t-val) 3))
        (setf (values diff) (mod (- p rem-val) 3))
        (return (+ t-val diff))))

(defun solve-state (x-val p-x y-val p-y)
  "Calculates the turns recursively jumping through quotient quotients O(log N)."
  (prog ((p-sum (- 3 p-x p-y))
         q r k x-prime y-prime p-x-prime v-0 v-k)
         
        (when (< x-val y-val)
          (return (solve-state y-val p-y x-val p-x)))
        (when (= x-val y-val)
          (return (next-turn 0 p-sum)))
    
        (setf (values q r) (floor x-val y-val))
    
        (cond ((= r 0)
               (setf (values k) (1- q))
               (setf (values x-prime) y-val)
               (setf (values y-prime) y-val))
              (t
               (setf (values k) q)
               (setf (values x-prime) r)
               (setf (values y-prime) y-val)))
           
        (cond ((evenp k)
               (setf (values p-x-prime) p-x))
              (t
               (setf (values p-x-prime) p-sum)))
           
        (setf (values v-0) (solve-state x-prime p-x-prime y-prime p-y))
    
        (cond ((evenp k)
               (setf (values v-k) (+ v-0 (floor (* 3 k) 2))))
              (t
               (setf (values v-k) (+ (next-turn v-0 p-sum) (floor (* 3 (1- k)) 2)))))
           
        (return v-k)))

(defun power (base exp)
  "Computes Bignum power recursively in prog state-machine."
  (prog ((res 1) (b base) (e exp))
L-LOOP  (when (<= e 0) (return res))
        (when (oddp e)
          (setf (values res) (* res b)))
        (setf (values b) (* b b))
        (setf (values e) (ash e -1))
        (go L-LOOP)))

(defun solve-euler-0905 ()
  "Orchestrates the O(133 * log N) computation completely bypassing the Red Line."
  (prog ((total 0)
         (a 1)
         (b 1)
         (a-val 0)
         (b-val 0)
         (f-val 0))

L-A     (when (> a 7) (return total))
        (setf (values b) 1)
L-B     (when (> b 19)
          (incf a)
          (go L-A))
        
        (setf (values a-val) (power a b))
        (setf (values b-val) (power b a))
        ;; C = A + B is always the sum holding player (p=2). 
        ;; Thus A is p=0, and B is p=1.
        (setf (values f-val) (solve-state a-val 0 b-val 1))
        (setf (values total) (+ total f-val))
      
        (incf b)
        (go L-B)))

(defun solve ()
  (format t "--- Mathematical Grounding Validation ---~%")
  ;; F(2,1,1) -> A=2 (p=0, sum), B=1 (p=1), C=1 (p=2) -> solve-state(B, C)
  (format t "Testing F(2, 1, 1)... Expected: 1, Got: ~A~%" (solve-state 1 1 1 2)) 
  ;; F(2,7,5) -> A=2 (p=0), B=7 (p=1, sum), C=5 (p=2) -> solve-state(A, C)
  (format t "Testing F(2, 7, 5)... Expected: 5, Got: ~A~%" (solve-state 2 0 5 2)) 
  (format t "-----------------------------------------~%")
  (format t "Solving for sum F(a^b, b^a, a^b+b^a) for a in 1..7, b in 1..19...~%")
  (let ((ans (solve-euler-0905)))
    (format t "Answer: ~A~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
--- Mathematical Grounding Validation ---
Testing F(2, 1, 1)... Expected: 1, Got: 1
Testing F(2, 7, 5)... Expected: 5, Got: 5
-----------------------------------------
Solving for sum F(a^b, b^a, a^b+b^a) for a in 1..7, b in 1..19...
Answer: 70228218

User time    =        0.000
System time  =        0.000
Elapsed time =        0.001
Allocation   = 864 bytes
0 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 70228218
:ok