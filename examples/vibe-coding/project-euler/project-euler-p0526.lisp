;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0526 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0526)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)

(defconstant +M+ 1062347) ; 11 * 13 * 17 * 19 * 23

;;; ----------------------------------------------------------------------------
;;; Candidate Structure for Max-Heap
;;; ----------------------------------------------------------------------------
(defstruct (candidate (:conc-name cand-))
  (k 0 :type integer)
  (t-val 0 :type integer)
  (r 0 :type fixnum)
  (coeffs nil :type list))

;;; Max-Heap Implementation
(defun heap-down (heap i n)
  (declare (type fixnum i n)
           (type (vector candidate) heap))
  (let ((cand (aref heap i))
        (k (cand-k (aref heap i))))
    (iterate
      (let ((child (+ (* 2 i) 1)))
        (when (>= child n) (leave))
        (when (and (< (1+ child) n)
                   (> (cand-k (aref heap (1+ child)))
                      (cand-k (aref heap child))))
          (incf child))
        (when (>= k (cand-k (aref heap child)))
          (leave))
        (setf (aref heap i) (aref heap child))
        (setf i child)))
    (setf (aref heap i) cand)))

(defun heap-up (heap i)
  (declare (type fixnum i)
           (type (vector candidate) heap))
  (let ((cand (aref heap i))
        (k (cand-k (aref heap i))))
    (iterate
      (when (zerop i) (leave))
      (let ((parent (truncate (1- i) 2)))
        (when (>= (cand-k (aref heap parent)) k)
          (leave))
        (setf (aref heap i) (aref heap parent))
        (setf i parent)))
    (setf (aref heap i) cand)))

(defun heap-push (heap cand)
  (vector-push-extend cand heap)
  (heap-up heap (1- (length heap))))

(defun heap-pop (heap)
  (let* ((n (length heap))
         (top (aref heap 0))
         (bottom (aref heap (1- n))))
    (decf (fill-pointer heap))
    (when (> (length heap) 0)
      (setf (aref heap 0) bottom)
      (heap-down heap 0 (1- n)))
    top))

;;; ----------------------------------------------------------------------------
;;; Deterministic Miller-Rabin for 64-bit integers
;;; ----------------------------------------------------------------------------
(defun modular-power (base exp m)
  (declare (type integer base exp m))
  (let ((res 1)
        (b (mod base m)))
    (iterate (while (> exp 0))
      (when (oddp exp)
        (setf res (mod (* res b) m)))
      (setf b (mod (* b b) m))
      (setf exp (ash exp -1)))
    res))

(defun miller-rabin (n)
  (declare (type integer n))
  (if (< n 2) (return-from miller-rabin nil))
  (if (or (= n 2) (= n 3)) (return-from miller-rabin t))
  (if (zerop (mod n 2)) (return-from miller-rabin nil))
  (let ((d (1- n))
        (s 0))
    (iterate (while (zerop (mod d 2)))
      (setf d (ash d -1))
      (incf s))
    ;; Bignum arithmetic handles up to 2^64 perfectly with these bases
    (iterate (for a in-vector '#(2 3 5 7 11 13 17 19 23 29 31 37))
      (when (>= a n) (leave))
      (let ((x (modular-power a d n)))
        (unless (or (= x 1) (= x (1- n)))
          (let ((prime-p nil))
            (iterate (for r from 1 below s)
              (setf x (mod (* x x) n))
              (when (= x 1) (return-from miller-rabin nil))
              (when (= x (1- n))
                (setf prime-p t)
                (leave)))
            (unless prime-p (return-from miller-rabin nil))))))
    t))

;;; ----------------------------------------------------------------------------
;;; Arithmetic & Algorithmic Shortcuts
;;; ----------------------------------------------------------------------------
(defun find-optimal-rs ()
  "Finds the remainder class 'r' (mod 2520) that maximizes the sum of coefficients.
$$\text{LCM}(1, 2, 3, 4, 5, 6, 7, 8, 9) = 2^3 \times 3^2 \times 5 \times 7 = 2520$$"
  (let ((max-v 0)
        (best-rs nil))
    (iterate (for r from 0 below 2520)
      (let ((v (iterate (for i from 0 to 8)
                 (sum (truncate 2520 (gcd (+ r i) 2520))))))
        (cond ((> v max-v)
               (setf max-v v)
               (setf best-rs (list r)))
              ((= v max-v)
               (push r best-rs)))))
    best-rs))

(defun get-coeffs (r)
  "Generates the 9 linear equations' coefficients (a_i . b_i) for a given 'r'."
  (let ((coeffs nil))
    (iterate (for i from 0 to 8)
      (let* ((c (gcd (+ r i) 2520))
             (a (truncate 2520 c))
             (b (truncate (+ r i) c)))
        (push (cons a b) coeffs)))
    (nreverse coeffs)))

(defun get-valid-ms (coeffs)
  "Applies the Wheel Sieve (M=1062347) to filter out t-remainders."
  (let ((valid (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t)))
    (iterate (for m from 0 below +M+)
      (declare (type fixnum m))
      (let ((ok t))
        (iterate (for (a . b) in coeffs)
          (let ((val (+ (* a m) b)))
            (declare (type integer val))
            (when (or (zerop (mod val 11))
                      (zerop (mod val 13))
                      (zerop (mod val 17))
                      (zerop (mod val 19))
                      (zerop (mod val 23)))
              (setf ok nil)
              (leave))))
        (when ok (vector-push-extend m valid))))
    valid))

;;; ----------------------------------------------------------------------------
;;; Main Solver
;;; ----------------------------------------------------------------------------
(defun solve ()
  (let* ((target-n 10000000000000000) ; 10^16
         (rs (find-optimal-rs))
         (heap (make-array 50000 :fill-pointer 0 :adjustable t)))
    
    (format t "[*] Phase 1: Found optimal remainder classes (mod 2520): ~A~%" rs)
    
    (iterate (for r in rs)
      (let* ((coeffs (get-coeffs r))
             (valid-ms (get-valid-ms coeffs))
             (max-t (truncate target-n 2520)))
        (format t "    r = ~D: Wheel Sieve reduced candidates to ~D valid mods modulo ~D~%" 
                r (length valid-ms) +M+)
        (iterate (for m in-vector valid-ms)
          (let* ((rem (mod (- max-t m) +M+))
                 (t-val (- max-t rem)))
            (when (<= t-val max-t)
              (let ((k (+ (* 2520 t-val) r)))
                (vector-push-extend (make-candidate :k k :t-val t-val :r r :coeffs coeffs) heap)))))))
    
    (format t "[*] Phase 2: Heapifying ~D initial candidates for descending search...~%" (length heap))
    (let ((n (length heap)))
      (iterate (for i from (truncate n 2) downto 0)
        (heap-down heap i n)))
    
    (format t "[*] Phase 3: Commencing top-down search for the 9-tuple prime constellation...~%")
    (let ((checked 0))
      (iterate
        (when (zerop (length heap))
          (format t "[-] Heap exhausted! No solution found.~%")
          (leave))
        (let* ((cand (heap-pop heap))
               (k (cand-k cand))
               (t-val (cand-t-val cand))
               (r (cand-r cand))
               (coeffs (cand-coeffs cand)))
          
          (incf checked)
          (when (zerop (mod checked 10000))
            (format t "    [Log] Checked ~D candidates, current descending k = ~D~%" checked k))
          
          ;; Early return pattern: Validating if all 9 linear forms yield primes
          (let ((all-prime t)
                (sum 0))
            (iterate (for (a . b) in coeffs)
              (let ((p (+ (* a t-val) b)))
                (if (miller-rabin p)
                    (incf sum p)
                    (progn
                      (setf all-prime nil)
                      (leave)))))
            (if all-prime
                (progn
                  (format t "~%[+] Mathematical Leap Successfully Resolved!~%")
                  (format t "    Target k = ~D~%" k)
                  (format t "    Maximum g(k) = ~D~%" sum)
                  (return-from solve sum))
                (progn
                  ;; If failed, subtract M to maintain valid modular property and push back
                  (let ((next-t (- t-val +M+)))
                    (when (> next-t 0)
                      (let ((next-k (+ (* 2520 next-t) r)))
                        (heap-push heap (make-candidate :k next-k :t-val next-t :r r :coeffs coeffs)))))))))))))


#+| Do it | (solve )
#|----------------------------------------------------------------
    [Log] Checked 18770000 candidates, current descending k = 9997195902085031

[+] Mathematical Leap Successfully Resolved!
    Target k = 9997194587108081
    Maximum g(k) = 49601160286750947

User time    =  0:10:35.757
System time  =        4.980
Elapsed time =  0:10:48.749
Allocation   = 104809297904 bytes
245300 Page faults
GC time      =        2.1052
;; →49601160286750947
----------------------------------------------------------------|#                   
:ok