;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0578 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0578)
(eval-when (:compile-toplevel :load-toplevel :execute) (series::install))

#||
  Absolute Dimension Collapse Protocol for Project Euler 578:
  - Reverts to the optimal u (square-full DPP) * v (square-free) decomposition.
  - S-func accurately calculates square-free combinations via mobius and phi-func.
  - THE MISSING KEY: Implements the O(1) mathematical shortcut in phi-func:
    If n < p_{k+1}^2, then phi(n, k) = 1 + pi(n) - k.
    This single invariant prevents the O(2^N) recursion explosion instantly.
  - Uses 61-bit fixnum bitwise keys for zero-allocation caching in phi-func.
||#

(defvar *x* (expt 10 13))
(defvar *max-p* 0)
(defvar *primes* (make-array 0 :element-type 'fixnum))
(defvar *pi-arr* (make-array 0 :element-type 'fixnum))
(defvar *mu* (make-array 0 :element-type 'fixnum))
(defvar *phi-cache* (make-hash-table :test 'eql))

(defun integer-root (n a)
  (declare (type fixnum n a)
           (optimize (speed 3) (safety 0)))
  (if (<= n 0) 0
      (let ((r (floor (expt (coerce n 'double-float) (/ 1.0d0 a)))))
        (declare (type fixnum r))
        (iterate ()
          (if (> (expt r a) n) (decf r) (terminate-producing)))
        (iterate ()
          (if (<= (expt (1+ r) a) n) (incf r) (terminate-producing)))
        r)))

(defun precompute (x)
  "Initializes primes, mobius function and pi array up to sqrt(x)."
  (setf *max-p* (isqrt x))
  (setf *pi-arr* (make-array (1+ *max-p*) :element-type 'fixnum :initial-element 0))
  (setf *mu* (make-array (1+ *max-p*) :element-type 'fixnum :initial-element 0))
  (setf (aref *mu* 1) 1)
  
  (let ((is-prime-bit (make-array (1+ *max-p*) :element-type 'bit :initial-element 1))
        (prime-list (make-array (1+ *max-p*) :fill-pointer 0 :element-type 'fixnum)))
    (setf (sbit is-prime-bit 0) 0)
    (setf (sbit is-prime-bit 1) 0)
    
    (iterate ((i (scan-range :from 2 :upto *max-p*)))
      (when (= (sbit is-prime-bit i) 1)
        (vector-push i prime-list)
        (setf (aref *mu* i) -1)
        (iterate ((j (scan-range :from (* i 2) :upto *max-p* :by i)))
          (setf (sbit is-prime-bit j) 0)
          (if (= (mod (floor j i) i) 0)
              (setf (aref *mu* j) 0)
              (setf (aref *mu* j) (* -1 (aref *mu* (floor j i))))))))
    
    (let ((p1 (make-array (1+ (length prime-list)) :element-type 'fixnum)))
      (setf (aref p1 0) 0)
      (iterate ((i (scan-range :from 0 :below (length prime-list))))
        (setf (aref p1 (1+ i)) (aref prime-list i)))
      (setf *primes* p1))
    
    (let ((count 0))
      (declare (type fixnum count))
      (iterate ((i (scan-range :from 1 :upto *max-p*)))
        (when (and (< count (1- (length *primes*)))
                   (>= i (aref *primes* (1+ count))))
          (incf count))
        (setf (aref *pi-arr* i) count))))
  (setf *phi-cache* (make-hash-table :test 'eql)))

(defun phi-func (n k)
  "Calculates integers <= n not divisible by first k primes. Uses O(1) mathematical shortcut to prevent explosion."
  (declare (type fixnum n k)
           (optimize (speed 3) (safety 0)))
  (cond ((<= n 0) 0)
        ((= k 0) n)
        ((= k 1) (- n (ash n -1)))
        (t
         (let ((p (aref *primes* k)))
           (declare (type fixnum p))
           (cond 
             ;; Trivial Case: n is smaller than the current prime. Only '1' survives.
             ((<= n p) 1)
             ;; THE MISSING SHORTCUT: If n < p_{k+1}^2, all composites are sifted. Only 1 and primes >= p_{k+1} remain.
             ((and (<= n *max-p*)
                   (let ((next-p (if (< (1+ k) (length *primes*))
                                     (aref *primes* (1+ k))
                                     (1+ n))))
                     (< n (* next-p next-p))))
              (+ 1 (- (aref *pi-arr* n) k)))
             ;; Recursive step with 61-bit Fixnum caching
             (t
              (let ((key (the fixnum (logior (ash k 45) n))))
                (multiple-value-bind (val found) (gethash key *phi-cache*)
                  (if found val
                      (setf (gethash key *phi-cache*)
                            (- (phi-func n (1- k))
                               (phi-func (floor n p) (1- k)))))))))))))

(defun S-func (V p-idx)
  "Calculates square-free integers <= V with all prime factors > primes[p-idx] via mobius inversion."
  (declare (type fixnum V p-idx)
           (optimize (speed 3) (safety 0)))
  (let ((p (aref *primes* p-idx)))
    (declare (type fixnum p))
    (if (<= V p)
        (if (>= V 1) 1 0)
        (let ((ans 0))
          (declare (type fixnum ans))
          (labels ((dfs-a (current-a start-p-idx current-mu)
                     (declare (type fixnum current-a start-p-idx current-mu))
                     (incf ans (* current-mu (phi-func (floor V (* current-a current-a)) p-idx)))
                     (iterate ((i (scan-range :from start-p-idx :below (length *primes*))))
                       (let* ((q (aref *primes* i))
                              (next-a (* current-a q)))
                         (declare (type fixnum q next-a))
                         (if (> next-a (isqrt V))
                             (terminate-producing)
                             (dfs-a next-a (1+ i) (- current-mu)))))))
            (dfs-a 1 (1+ p-idx) 1))
          ans))))

(defun S-func-p0 (V)
  "Calculates all square-free integers <= V."
  (declare (type fixnum V)
           (optimize (speed 3) (safety 0)))
  (let ((limit (isqrt V)))
    (declare (type fixnum limit))
    (collect-sum
     (mapping ((a (scan-range :from 1 :upto limit)))
       (let ((m (aref *mu* a)))
         (declare (type fixnum m))
         (if (/= m 0)
             (* m (floor V (* a a)))
             0))))))

(defun dfs-u (current-u last-p-idx max-a)
  "Explores square-full DPP cores u, dynamically branching valid prime powers."
  (declare (type fixnum current-u last-p-idx max-a)
           (optimize (speed 3) (safety 0)))
  (let* ((last-p (if (= last-p-idx 0) 0 (aref *primes* last-p-idx)))
         (ans 0))
    (declare (type fixnum last-p ans))
    
    ;; Power a = 2: Trigger bulk-counting branch pruning
    (let* ((limit-q (isqrt (floor *x* current-u)))
           (threshold-q (max last-p (integer-root (floor *x* current-u) 3))))
      (declare (type fixnum limit-q threshold-q))
      (when (> limit-q threshold-q)
        (incf ans (- (aref *pi-arr* limit-q) (aref *pi-arr* threshold-q))))
      
      (let ((loop-end (min limit-q threshold-q)))
        (declare (type fixnum loop-end))
        (iterate ((i (scan-range :from (1+ last-p-idx) :below (length *primes*))))
          (let ((q (aref *primes* i)))
            (declare (type fixnum q))
            (if (> q loop-end)
                (terminate-producing)
                (let ((new-u (* current-u q q)))
                  (declare (type fixnum new-u))
                  (incf ans (S-func (floor *x* new-u) i))
                  (incf ans (dfs-u new-u i 2))))))))
    
    ;; Powers a >= 3
    (iterate ((a (scan-range :from 3 :upto max-a)))
      (let ((limit-q-a (integer-root (floor *x* current-u) a)))
        (declare (type fixnum limit-q-a))
        (if (<= limit-q-a last-p)
            (terminate-producing)
            (iterate ((i (scan-range :from (1+ last-p-idx) :below (length *primes*))))
              (let ((q (aref *primes* i)))
                (declare (type fixnum q))
                (if (> q limit-q-a)
                    (terminate-producing)
                    (let ((new-u (* current-u (expt q a))))
                      (declare (type fixnum new-u))
                      (incf ans (S-func (floor *x* new-u) i))
                      (incf ans (dfs-u new-u i a)))))))))
    ans))

(defun solve ()
  (format t "-> Initializing Sieves up to sqrt(10^13)...~%")
  (precompute *x*)
  (format t "-> Sieves complete. Exploring DPP core structures with Phi-Shortcut...~%")
  (let ((result (+ (S-func-p0 *x*)
                   (dfs-u 1 0 most-positive-fixnum))))
    (format t "-> Search complete.~%")
    result))

#+| Do it | (project-euler-0578:solve)