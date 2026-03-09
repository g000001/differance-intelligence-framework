;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0579 (:use cl iterate alexandria))
(in-package #:project-euler-0579)

#||
(cl-text euler-579-acx
  (cl-comment "Ontology for Euler 579: Lattice Cubes")
  
  (cl-comment "1. NMF Prevention: Eliminating overcounting and missing states")
  (forall (cube)
    (iff (is-primitive-lattice-cube cube)
         (exists (a b c d)
           (and (integer a b c d)
                (= 1 (gcd a b c d))
                (generates-orthogonal-basis a b c d cube)))))

  (cl-comment "2. ACX Jump: Ehrhart Polynomial for Orthogonal Zonotopes")
  (cl-comment "Cross product GCD simplifies: gcd(u x v) = L * gcd(w)")
  (forall (L u v w P)
    (if (is-orthogonal-basis L u v w)
        (= P (+ (expt L 3) 
                (* (+ L 1) (+ (gcd u) (gcd v) (gcd w))) 
                1))))

  (cl-comment "3. Middle-Way Manifestation: Box projection")
  (forall (u v w Bx By Bz)
    (if (is-basis u v w)
        (and (= Bx (+ (abs (x u)) (abs (x v)) (abs (x w))))
             (= By (+ (abs (y u)) (abs (y v)) (abs (y w))))
             (= Bz (+ (abs (z u)) (abs (z v)) (abs (z w)))))))
)
||#

(defun solve (&optional (n 5000))
  "Optimized solver using exact quaternion space and Ehrhart polynomials."
  (let ((total-sum 0)
        (mod-val 1000000000)
        (seen (make-hash-table :test 'equal))
        ;; L_quat <= 2N is sufficient to find at least one representation for every primitive cube
        (limit2 (* 2 n)))
    
    ;; Iterate over projective quaternion space P^3(Q) exactly once
    (iterate (for a from 0 to (isqrt limit2))
      (let* ((a2 (* a a))
             (b-min (if (= a 0) 0 (- (isqrt (- limit2 a2))))))
        (iterate (for b from b-min to (isqrt (- limit2 a2)))
          (let* ((a2b2 (+ a2 (* b b)))
                 (c-min (if (and (= a 0) (= b 0)) 0 (- (isqrt (- limit2 a2b2))))))
            (iterate (for c from c-min to (isqrt (- limit2 a2b2)))
              (let* ((a2b2c2 (+ a2b2 (* c c)))
                     (d-min (if (and (= a 0) (= b 0) (= c 0)) 1 (- (isqrt (- limit2 a2b2c2))))))
                (iterate (for d from d-min to (isqrt (- limit2 a2b2c2)))
                  
                  ;; Ensure primitive quaternion
                  (when (= 1 (gcd a (gcd (abs b) (gcd (abs c) (abs d)))))
                    (let* ((m11 (+ a2 (* b b) (- (* c c)) (- (* d d))))
                           (m12 (* 2 (- (* b c) (* a d))))
                           (m13 (* 2 (+ (* b d) (* a c))))
                           (m21 (* 2 (+ (* b c) (* a d))))
                           (m22 (+ a2 (- (* b b)) (* c c) (- (* d d))))
                           (m23 (* 2 (- (* c d) (* a b))))
                           (m31 (* 2 (- (* b d) (* a c))))
                           (m32 (* 2 (+ (* c d) (* a b))))
                           (m33 (+ a2 (- (* b b)) (- (* c c)) (* d d)))
                           ;; GCD of the entire matrix
                           (g (gcd m11 (gcd m12 (gcd m13 (gcd m21 (gcd m22 (gcd m23 (gcd m31 (gcd m32 m33))))))))))
                      
                      ;; Extract primitive basis vectors u0, v0, w0
                      (let* ((u0x (/ m11 g)) (u0y (/ m21 g)) (u0z (/ m31 g))
                             (v0x (/ m12 g)) (v0y (/ m22 g)) (v0z (/ m32 g))
                             (w0x (/ m13 g)) (w0y (/ m23 g)) (w0z (/ m33 g))
                             ;; Bounding box sizes
                             (bx0 (+ (abs u0x) (abs v0x) (abs w0x)))
                             (by0 (+ (abs u0y) (abs v0y) (abs w0y)))
                             (bz0 (+ (abs u0z) (abs v0z) (abs w0z)))
                             (bmax (max bx0 by0 bz0)))
                        
                        ;; Only process if it fits in the N-box at least once
                        (when (<= bmax n)
                          ;; Create canonical key to eliminate rotations/reflections
                          (let* ((u0 (list u0x u0y u0z))
                                 (v0 (list v0x v0y v0z))
                                 (w0 (list w0x w0y w0z))
                                 (mu0 (list (- u0x) (- u0y) (- u0z)))
                                 (mv0 (list (- v0x) (- v0y) (- v0z)))
                                 (mw0 (list (- w0x) (- w0y) (- w0z)))
                                 (edges (list u0 mu0 v0 mv0 w0 mw0)))
                            (setf edges (sort edges (lambda (e1 e2)
                                                      (cond ((< (first e1) (first e2)) t)
                                                            ((> (first e1) (first e2)) nil)
                                                            ((< (second e1) (second e2)) t)
                                                            ((> (second e1) (second e2)) nil)
                                                            ((< (third e1) (third e2)) t)
                                                            (t nil)))))
                            
                            (unless (gethash edges seen)
                              (setf (gethash edges seen) t)
                              
                              (let* ((L0 (/ (+ a2 (* b b) (* c c) (* d d)) g))
                                     (gu0 (gcd u0x (gcd u0y u0z)))
                                     (gv0 (gcd v0x (gcd v0y v0z)))
                                     (gw0 (gcd w0x (gcd w0y w0z)))
                                     (Sg (+ gu0 gv0 gw0)))
                                
                                ;; Scale the primitive cube k times up to N
                                (iterate (for k from 1 to (floor n bmax))
                                  (let* ((Lk (* k L0))
                                         ;; Exact Ehrhart evaluation: P = L^3 + (L+1)*(gu+gv+gw) + 1
                                         (P (+ (* Lk Lk Lk)
                                               (* (1+ Lk) (* k Sg))
                                               1))
                                         ;; Number of valid translations in N-box
                                         (W (mod (* (1+ (- n (* k bx0)))
                                                    (1+ (- n (* k by0)))
                                                    (1+ (- n (* k bz0))))
                                                 mod-val)))
                                    (setf total-sum (mod (+ total-sum (mod (* W P) mod-val)) mod-val))))))))))))))))))
    (mod total-sum mod-val)))


#+| Do it | (solve )
#|
→ 3805524 
User time    =  0:05:33.035
System time  =        6.290
Elapsed time =  0:05:47.074
Allocation   = 24537489096 bytes
938580 Page faults
GC time      =        5.1155
|#
:ok