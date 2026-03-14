;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0252 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0252)

#||
(cl-text EULER-ACX-DIFD-INTEGRATION
  (cl-comment "
  =============================================================================
  ARX-Core: Structural Gravity Protocol for PE 0252 (Discrete DIFD)
  =============================================================================
  Formalization of the Alethetic Reset: Transitioning from O(2^N) subset 
  brute-force to O(N^4) dynamic programming anchored by radial sorting.
  ")

  (cl-comment "1. NMF (Non-Middle Fallacy) Detection")
  (forall (?solver)
    (if (and (Solves ?solver PE0252)
             (Explores ?solver (PowerSetOf Points)))
        (and (NMF ?solver)
             (ProducesHallucination (CombinatorialExplosion ?solver))
             (ExceedsTimeLimit 60))))

  (cl-comment "2. ACX Jump: Orthogonal Projection via Anchor Point and Radial Sorting")
  (cl-comment "By fixing the bottom-leftmost vertex V0 of the polygon, the vertices
               must appear in strict counter-clockwise order. This projects the 2D
               problem onto a 1D sequence. The 'empty hole' constraint is locally 
               enforced by verifying that no point blocks the visibility between 
               two consecutive vertices A_i and A_j.")
  (forall (?V0 ?Ai ?Aj)
    (iff (ValidEdge ?V0 ?Ai ?Aj)
         (and (RadiallySorted ?V0 ?Ai ?Aj)
              (Not (Exists (?Ak)
                     (and (RadiallyBetween ?Ak ?Ai ?Aj)
                          (IsLeftOf ?Ak (DirectedEdge ?Ai ?Aj))))))))

  (cl-comment "3. Middle Way Manifestation")
  (cl-comment "Precomputing ValidEdge in O(M^2) for each anchor, the maximum empty
               convex polygon can be found using DP in O(M^3). Since M <= N, the 
               overall complexity is O(N^4), traversing ~10^9 highly optimized 
               integer operations safely within 60 seconds.")
)
||#

(defun get-points (num)
  "Generate unique points using the specified pseudo-random number generator."
  (declare (type fixnum num))
  (let ((s 290797)
        (pts nil))
    (declare (type fixnum s))
    (iterate (for k from 1 to num)
      (setf s (mod (* s s) 50515093))
      (let ((x (- (mod s 2000) 1000)))
        (declare (type fixnum x))
        (setf s (mod (* s s) 50515093))
        (let ((y (- (mod s 2000) 1000)))
          (declare (type fixnum y))
          (push (cons x y) pts))))
    ;; The set of points must have unique coordinates
    (remove-duplicates pts :test #'equal)))

(defun sort-radially (pts v0x v0y)
  "Sort points radially around (v0x, v0y) in counter-clockwise order."
  (declare (type fixnum v0x v0y))
  (sort pts
        (lambda (a b)
          (let* ((ax (- (car a) v0x))
                 (ay (- (cdr a) v0y))
                 (bx (- (car b) v0x))
                 (by (- (cdr b) v0y))
                 (cross (- (* ax by) (* ay bx))))
            (declare (type fixnum ax ay bx by cross))
            (if (not (zerop cross))
                (> cross 0)
                ;; If collinear, the closer point comes first
                (< (+ (* ax ax) (* ay ay))
                   (+ (* bx bx) (* by by))))))))

(defun solve ()
  "Finds the maximum area of a convex hole in the pseudo-random point set."
  (let* ((pts (get-points 500))
         (max-ans 0)
         (n (length pts))
         ;; Preallocate flat arrays to prevent GC allocations in the tight loop
         (valid (make-array (* n n) :element-type 'bit :initial-element 0))
         (dp (make-array (* n n) :element-type 'fixnum :initial-element 0))
         (ax (make-array n :element-type 'fixnum))
         (ay (make-array n :element-type 'fixnum)))
    (declare (type fixnum max-ans n)
             (type (simple-array bit (*)) valid)
             (type (simple-array fixnum (*)) dp ax ay)
             (optimize (speed 3) (safety 0) (debug 0)))
    
    (iterate (for v0 in pts)
      (let ((v0x (car v0))
            (v0y (cdr v0))
            (above-pts nil))
        (declare (type fixnum v0x v0y))
        
        ;; Select points strictly "above" v0 to enforce v0 as the unique anchor
        (iterate (for p in pts)
          (let ((px (car p)) (py (cdr p)))
            (declare (type fixnum px py))
            (when (or (> py v0y)
                      (and (= py v0y) (> px v0x)))
              (push p above-pts))))
        
        (setf above-pts (sort-radially above-pts v0x v0y))
        (let ((m (length above-pts)))
          (declare (type fixnum m))
          (when (< m 2) (next-iteration))
          
          ;; Extract relative coordinates
          (iterate (for p in above-pts)
                   (for i from 0)
            (setf (aref ax i) (- (car p) v0x))
            (setf (aref ay i) (- (cdr p) v0y)))
          
          ;; Reset dp array and valid edges for current anchor
          (iterate (for i from 0 below (* m m))
            (setf (aref dp i) 0)
            (setf (sbit valid i) 0))
          
          ;; Step 1: Compute valid edges. An edge (i, j) is valid if no point k (i < k < j)
          ;; is strictly to the left of the directed segment from A_i to A_j.
          ;; Because of the angular sorting, we only need to track the "rightmost" point (max angle).
          (iterate (for i from 0 below m)
            (let ((p-max-dx 0) (p-max-dy 0) (has-max nil))
              (declare (type fixnum p-max-dx p-max-dy))
              (iterate (for j from (1+ i) below m)
                (let ((dx (- (aref ax j) (aref ax i)))
                      (dy (- (aref ay j) (aref ay i))))
                  (declare (type fixnum dx dy))
                  ;; CCW check against the running restrictor point
                  (if (or (not has-max)
                          (>= (- (* p-max-dx dy) (* p-max-dy dx)) 0))
                      (progn
                        (setf (sbit valid (+ (* i n) j)) 1)
                        (setf p-max-dx dx p-max-dy dy has-max t))
                      (setf (sbit valid (+ (* i n) j)) 0))))))
          
          ;; Step 2: Dynamic Programming to find max area empty convex chain ending at (i, j)
          (iterate (for j from 1 below m)
            (let ((ax-j (aref ax j)) (ay-j (aref ay j)))
              (declare (type fixnum ax-j ay-j))
              (iterate (for i from 0 below j)
                (when (= 1 (sbit valid (+ (* i n) j)))
                  (let ((mx 0)
                        (ax-i (aref ax i))
                        (ay-i (aref ay i)))
                    (declare (type fixnum mx ax-i ay-i))
                    (let ((dx-ij (- ax-j ax-i))
                          (dy-ij (- ay-j ay-i)))
                      (declare (type fixnum dx-ij dy-ij))
                      (iterate (for h from 0 below i)
                        (when (= 1 (sbit valid (+ (* h n) i)))
                          (let ((dx-hi (- ax-i (aref ax h)))
                                (dy-hi (- ay-i (aref ay h))))
                            (declare (type fixnum dx-hi dy-hi))
                            ;; Convexity check: left-turn from A_h -> A_i -> A_j
                            (when (>= (- (* dx-hi dy-ij) (* dy-hi dx-ij)) 0)
                              (let ((val (aref dp (+ (* h n) i))))
                                (declare (type fixnum val))
                                (when (> val mx)
                                  (setf mx val))))))))
                    
                    ;; Area added is CCW(V0, A_i, A_j). Since points are relative to V0,
                    ;; it is simply the 2D cross product of A_i and A_j.
                    (let ((area (+ mx (- (* ax-i ay-j) (* ay-i ax-j)))))
                      (declare (type fixnum area))
                      (setf (aref dp (+ (* i n) j)) area)
                      (when (> area max-ans)
                        (setf max-ans area)))))))))))
    
    ;; Format the result: max-ans contains 2 * MaxArea.
    (format nil "~D.~D" (ash max-ans -1) (if (oddp max-ans) 5 0))))


#+| Do it | (solve )