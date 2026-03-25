;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0362 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0362)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

(defvar *mu* (make-array 0 :element-type 'fixnum))
(defvar *q-small* (make-array 0 :element-type 'fixnum))
(defvar *q-large* (make-array 0 :element-type 'fixnum))
(defvar *sqf-list* (make-array 0 :element-type 'fixnum))
(defvar *n* 0)
(defvar *r* 0)

(defun init-buffers (n)
  (setf *n* n)
  (setf *r* (isqrt n))
  (let ((size (1+ *r*)))
    (when (< (length *mu*) size)
      (setf *mu* (make-array size :element-type 'fixnum :initial-element 0))
      (setf *q-small* (make-array size :element-type 'fixnum :initial-element 0))
      (setf *q-large* (make-array size :element-type 'fixnum :initial-element 0)))))

(declaim (inline get-q))
(defun get-q (x p)
  "Returns the number of squarefree integers <= x in O(1)."
  (declare (type fixnum x p))
  (if (<= x *r*)
      (aref *q-small* x)
      (aref *q-large* p)))


(defun dfs (x p q-idx)
  "Recursively counts the valid squarefree factorizations without allocations."
  (declare (type fixnum x p q-idx))
  (let ((q-start (aref *sqf-list* q-idx)))
    (declare (type fixnum q-start))
    (if (< x q-start)
        0
        (let ((ans (- (get-q x p) (aref *q-small* (1- q-start)))))
          (declare (type fixnum ans))
          (let* ((max-q (isqrt x))
                 ;; O(1) index lookup exploiting the cumulative sum property
                 (end-idx (- (aref *q-small* max-q) 2)))
            (declare (type fixnum max-q end-idx))
            (if (< end-idx q-idx)
                ans
                (+ ans
                   (collect-sum 
                    (mapping ((i (scan-range :from q-idx :upto end-idx)))
                      (declare (type fixnum i))
                      (let ((q (aref *sqf-list* i)))
                        (declare (type fixnum q))
                        ;; State transition: x' = floor(x/q), P' = P*q
                        (dfs (truncate x q) (* p q) i)))
                    'fixnum))))))))

(defun solve-for (n)
  (init-buffers n)
  (let ((r *r*))
    (declare (type fixnum r))
    
    ;; 1. Proper Linear Sieve for Mobius function
    (let ((is-prime (make-array (1+ r) :element-type 'bit :initial-element 1))
          (primes (make-array r :element-type 'fixnum :fill-pointer 0)))
      (setf (aref is-prime 0) 0
            (aref is-prime 1) 0
            (aref *mu* 1) 1)
      (iterate ((i (scan-range :from 2 :upto r)))
        (when (= (aref is-prime i) 1)
          (setf (aref *mu* i) -1)
          (vector-push i primes))
        (iterate ((j (scan-range :from 0 :below (length primes))))
          (let* ((p (aref primes j))
                 (p*i (* p i)))
            (declare (type fixnum p p*i))
            (if (> p*i r)
                (terminate-producing)
                (progn
                  (setf (aref is-prime p*i) 0)
                  (if (zerop (mod i p))
                      (progn
                        (setf (aref *mu* p*i) 0)
                        (terminate-producing))
                      (setf (aref *mu* p*i) (- (aref *mu* i))))))))))
                      
    ;; 2. Build Q-small (Cumulative sum of squarefree numbers)
    (let ((sqf-count 0))
      (declare (type fixnum sqf-count))
      (iterate ((i (scan-range :from 1 :upto r)))
        (when (not (zerop (aref *mu* i)))
          (incf sqf-count))
        (setf (aref *q-small* i) sqf-count)))
        
    ;; 3. Extract the list of squarefree numbers >= 2
    (let ((count (- (aref *q-small* r) 1)))
      (setf *sqf-list* (make-array count :element-type 'fixnum))
      (let ((idx 0))
        (declare (type fixnum idx))
        (iterate ((i (scan-range :from 2 :upto r)))
          (when (not (zerop (aref *mu* i)))
            (setf (aref *sqf-list* idx) i)
            (incf idx)))))
            
    ;; 4. Build Q-large (Number of squarefree numbers <= floor(N/v))
    ;; Total operations bounded tightly to ~60 million
    (iterate ((v (scan-range :from 1 :upto r)))
      (let* ((x (truncate n v))
             (max-i (isqrt x))
             (sum 0))
        (declare (type fixnum x max-i sum))
        (iterate ((i (scan-range :from 1 :upto max-i)))
          (let ((m (aref *mu* i)))
            (declare (type fixnum m))
            (when (not (zerop m))
              (let ((term (truncate x (* i i))))
                (declare (type fixnum term))
                (if (> m 0)
                    (incf sum term)
                    (decf sum term))))))
        (setf (aref *q-large* v) sum)))
        
    ;; 5. Execute DFS
    (dfs n 1 0)))

(defun solve ()
  (format t "Validating with S(100)...~%")
  (let ((test-ans (solve-for 100)))
    (format t "S(100) = ~A (Expected 193)~%" test-ans)
    (assert (= test-ans 193)))
    
  (format t "Computing S(10,000,000,000)...~%")
  (let ((final-ans (solve-for 10000000000)))
    (format t "Final Answer S(10^10) = ~A~%" final-ans)
    final-ans))

#+| Do it | (project-euler-0362:solve)
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Validating with S(100)...
S(100) = 193 (Expected 193)
Computing S(10,000,000,000)...
Final Answer S(10^10) = 457895958010

User time    =       46.218
System time  =        0.409
Elapsed time =       46.721
Allocation   = 1925136 bytes
7736 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 457895958010
:ok