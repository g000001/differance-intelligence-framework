;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0909 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0909)

#||
(cl:comment "PE 909 CLIF Logic Definition")
(forall (E n)
  (iff (= (val E n) (eval-L-expr E n))
       (and (= (val Z n) 1)
            (= (val S n) (+ n 1))
            (= (val (S U) n) (* n (val U n)))
            (= (val ((S U) V) n) (val V (val (U V) n))))))
||#


(defun solve ()
  (format t "Evaluating polynomial mapping of the L-expression...~%")
  
  (let* ((f (lambda (n) (* n (1+ n))))
         (f2 (lambda (n) (funcall f (* n (funcall f n)))))
         (y (lambda (n) (funcall f (funcall f2 n))))
         ;; The final answer is y(y(1)) based on the mathematical reduction
         (raw-ans (funcall y (funcall y 1)))
         (mod-ans (mod raw-ans 1000000000)))
    
    (format t "Raw Answer (Bignum): ~A~%" raw-ans)
    (format t "Last 9 digits: ~9,'0D~%" mod-ans)
    mod-ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Evaluating polynomial mapping of the L-expression...
Raw Answer (Bignum): 33103933172399885292
Last 9 digits: 399885292

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 408 bytes
47 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 399885292
:ok