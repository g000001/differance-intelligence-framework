;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0715 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0715)

(defparameter *mod* 1000000007)

(declaim (inline sum-chi sum-cubes))
(defun sum-chi (n)
  "Sum of chi(d) for all integers up to n. Since chi(even)=0, this correctly sums odd d."
  (declare (type fixnum n))
  (let ((rem (mod n 4)))
    (cond ((= rem 0) 0)
          ((= rem 1) 1)
          ((= rem 2) 1)
          (t 0))))

(defun sum-cubes (n)
  "Sum of k^3 up to n modulo *mod*."
  (declare (type fixnum n))
  (let* ((n-mod (mod n *mod*))
         (term (mod (* n-mod (1+ n-mod)) *mod*))
         (half (mod (* term 500000004) *mod*)))
    (mod (* half half) *mod*)))

(defun solve ()
  (let* ((n 1000000000000)
         (k-limit 20000000) ; Precompute up to 2 * 10^7
         (h-arr (make-array (1+ k-limit) :element-type 'fixnum :initial-element 0))
         (is-prime (make-array (1+ k-limit) :element-type 'bit :initial-element 1))
         (primes (make-array 1300000 :element-type 'fixnum :fill-pointer 0)))
    (declare (type fixnum n k-limit))
    
    (format t "Step 1: Linear Sieve up to ~A...~%" k-limit)
    (setf (aref h-arr 1) 1)
    (setf (bit is-prime 0) 0)
    (setf (bit is-prime 1) 0)
    
    (iterate (for i from 2 to k-limit)
      (declare (type fixnum i))
      (when (= (bit is-prime i) 1)
        (vector-push i primes)
        (let ((i3 (mod (* i (mod (* i i) *mod*)) *mod*)))
          (declare (type fixnum i3))
          (cond ((= i 2) (setf (aref h-arr i) i3))
                ((= (mod i 4) 1) (setf (aref h-arr i) (mod (- i3 1) *mod*)))
                (t (setf (aref h-arr i) (mod (+ i3 1) *mod*))))))
      
      (iterate (for j from 0 below (fill-pointer primes))
        (declare (type fixnum j))
        (let* ((p (aref primes j))
               (ip (* i p)))
          (declare (type fixnum p ip))
          (when (> ip k-limit) (finish))
          (setf (bit is-prime ip) 0)
          (if (= (mod i p) 0)
              (let ((p3 (mod (* p (mod (* p p) *mod*)) *mod*)))
                (declare (type fixnum p3))
                ;; The magic invariant: H(p^a * m) = p^3 * H(p^{a-1} * m)
                (setf (aref h-arr ip) (mod (* p3 (aref h-arr i)) *mod*))
                (finish))
              (setf (aref h-arr ip) (mod (* (aref h-arr i) (aref h-arr p)) *mod*))))))
              
    (format t "Step 2: Prefix Sums...~%")
    (iterate (for i from 2 to k-limit)
      (declare (type fixnum i))
      (setf (aref h-arr i) (mod (+ (aref h-arr (1- i)) (aref h-arr i)) *mod*)))
      
    (format t "Step 3: Dirichlet Recurrence for G(10^12)...~%")
    (let* ((max-k (floor n (1+ k-limit)))
           (g-memo (make-array (1+ max-k) :element-type 'fixnum :initial-element -1)))
      (declare (type fixnum max-k))
      
      (iterate (for k from max-k downto 1)
        (declare (type fixnum k))
        (let* ((x (floor n k))
               (u (isqrt x))
               (ans (sum-cubes x)))
          (declare (type fixnum x u ans))
          
          ;; Part 1: d <= u
          (iterate (for d from 3 to u by 2)
            (declare (type fixnum d))
            (let* ((val (floor x d))
                   (g-val (if (<= val k-limit) (aref h-arr val) (aref g-memo (floor n val))))
                   (c (if (= (mod d 4) 1) 1 -1)))
              (declare (type fixnum val g-val c))
              (if (= c 1)
                  (setf ans (mod (- ans g-val) *mod*))
                  (setf ans (mod (+ ans g-val) *mod*)))))
                  
          ;; Part 2: grouped by m = floor(x/d)
          (let ((max-m (floor x (1+ u))))
            (declare (type fixnum max-m))
            (iterate (for m from 1 to max-m)
              (declare (type fixnum m))
              (let* ((right-bnd (floor x m))
                     (left-bnd (floor x (1+ m))))
                (declare (type fixnum right-bnd left-bnd))
                (when (< left-bnd u) (setf left-bnd u))
                (when (< left-bnd right-bnd)
                  (let ((c-sum (- (sum-chi right-bnd) (sum-chi left-bnd))))
                    (declare (type fixnum c-sum))
                    (unless (zerop c-sum)
                      (let ((g-val (if (<= m k-limit) (aref h-arr m) (aref g-memo (floor n m)))))
                        (declare (type fixnum g-val))
                        (setf ans (mod (- ans (* c-sum g-val)) *mod*)))))))))
          
          (setf (aref g-memo k) (mod (+ ans *mod*) *mod*))
          (when (zerop (mod k 10000))
            (format t "  k = ~A processed...~%" k))))
      
      (format t "Done.~%")
      (format nil "~A" (aref g-memo 1)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: Linear Sieve up to 20000000...
Step 2: Prefix Sums...
Step 3: Dirichlet Recurrence for G(10^12)...
  k = 40000 processed...
  k = 30000 processed...
  k = 20000 processed...
  k = 10000 processed...
Done.

User time    =       52.625
System time  =        0.120
Elapsed time =       52.842
Allocation   = 173523432 bytes
41716 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ "883188017"
:ok