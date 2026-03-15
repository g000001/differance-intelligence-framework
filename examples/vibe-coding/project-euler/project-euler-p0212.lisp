;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0212 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0212)

(defun solve (&optional (n-cuboids 50000))
  (format t "Step 1: Generating Fibonacci sequence...~%")
  (let* ((s (make-array 300001 :element-type 'fixnum))
         (cx0 (make-array n-cuboids :element-type 'fixnum))
         (cx1 (make-array n-cuboids :element-type 'fixnum))
         (cy0 (make-array n-cuboids :element-type 'fixnum))
         (cy1 (make-array n-cuboids :element-type 'fixnum))
         (cz0 (make-array n-cuboids :element-type 'fixnum))
         (cz1 (make-array n-cuboids :element-type 'fixnum)))

    ;; Fibonacci sequence generation
    (iterate (for k from 1 to 55)
             (let ((k-mod (mod k 1000000)))
               (setf (aref s k)
                     (mod (+ 100003
                             (- (mod (* 200003 k-mod) 1000000))
                             (mod (* 300007 (mod (* k-mod k-mod k-mod) 1000000)) 1000000))
                          1000000))))
    (iterate (for k from 56 to 300000)
             (setf (aref s k)
                   (mod (+ (aref s (- k 24)) (aref s (- k 55))) 1000000)))

    (format t "Step 2: Building cuboids parameters...~%")
    (iterate (for n from 1 to n-cuboids)
             (let* ((x0 (mod (aref s (- (* 6 n) 5)) 10000))
                    (y0 (mod (aref s (- (* 6 n) 4)) 10000))
                    (z0 (mod (aref s (- (* 6 n) 3)) 10000))
                    (dx (+ 1 (mod (aref s (- (* 6 n) 2)) 399)))
                    (dy (+ 1 (mod (aref s (- (* 6 n) 1)) 399)))
                    (dz (+ 1 (mod (aref s (* 6 n)) 399))))
               (let ((idx (- n 1)))
                 (setf (aref cx0 idx) x0)
                 (setf (aref cx1 idx) (+ x0 dx))
                 (setf (aref cy0 idx) y0)
                 (setf (aref cy1 idx) (+ y0 dy))
                 (setf (aref cz0 idx) z0)
                 (setf (aref cz1 idx) (+ z0 dz)))))

    (format t "Step 3: Initializing sweep line event buckets...~%")
    (let* ((max-z 10400)
           (max-x 10400)
           ;; Z-axis buckets for adding and removing active cuboids
           (z-head-add (make-array max-z :element-type 'fixnum :initial-element -1))
           (z-next-add (make-array n-cuboids :element-type 'fixnum :initial-element -1))
           (z-head-rem (make-array max-z :element-type 'fixnum :initial-element -1))
           (z-next-rem (make-array n-cuboids :element-type 'fixnum :initial-element -1)))

      ;; Populate Z-events
      (iterate (for n from 0 below n-cuboids)
               (let ((z0 (aref cz0 n))
                     (z1 (aref cz1 n)))
                 (setf (aref z-next-add n) (aref z-head-add z0))
                 (setf (aref z-head-add z0) n)
                 (setf (aref z-next-rem n) (aref z-head-rem z1))
                 (setf (aref z-head-rem z1) n)))

      (let* ((active-ids (make-array n-cuboids :element-type 'fixnum))
             (active-pos (make-array n-cuboids :element-type 'fixnum))
             (active-count 0)
             
             ;; X-axis buckets for the sweep line inside each Z-slice
             (x-head (make-array max-x :element-type 'fixnum :initial-element -1))
             (max-events (* n-cuboids 2))
             (x-next (make-array max-events :element-type 'fixnum :initial-element -1))
             (x-is-add (make-array max-events :element-type 'fixnum))
             (x-y0 (make-array max-events :element-type 'fixnum))
             (x-y1 (make-array max-events :element-type 'fixnum))
             
             ;; Y-axis segment tree
             (tree-count (make-array 32768 :element-type 'fixnum :initial-element 0))
             (tree-length (make-array 32768 :element-type 'fixnum :initial-element 0))
             (total-volume 0))

        (labels
            ;; Using labels allows recursive segment tree updates with typed arguments
            ;; without the need for performance declaims that can cause bugs.
            ((update-tree (node l r ql qr delta)
               (declare (type fixnum node l r ql qr delta))
               (when (and (<= ql l) (<= r qr))
                 (incf (aref tree-count node) delta)
                 ;; Update covered length for this node
                 (if (> (aref tree-count node) 0)
                     (setf (aref tree-length node) (- r l))
                     (if (= (- r l) 1)
                         (setf (aref tree-length node) 0)
                         (setf (aref tree-length node)
                               (+ (aref tree-length (ash node 1))
                                  (aref tree-length (logior (ash node 1) 1))))))
                 (return-from update-tree))
               
               (let ((mid (ash (+ l r) -1))
                     (left (ash node 1))
                     (right (logior (ash node 1) 1)))
                 (when (< ql mid)
                   (update-tree left l mid ql qr delta))
                 (when (< mid qr)
                   (update-tree right mid r ql qr delta))
                   
                 (if (> (aref tree-count node) 0)
                     (setf (aref tree-length node) (- r l))
                     (if (= (- r l) 1)
                         (setf (aref tree-length node) 0)
                         (setf (aref tree-length node)
                               (+ (aref tree-length left)
                                  (aref tree-length right))))))))

          (format t "Step 4: Sweeping Z and X axis...~%")
          (iterate (for z from 0 below max-z)
                   
                   ;; 1. Process Z-events: Remove exiting cuboids
                   (let ((rem-idx (aref z-head-rem z)))
                     (iterate (while (>= rem-idx 0))
                              (let* ((pos (aref active-pos rem-idx))
                                     (last-id (aref active-ids (1- active-count))))
                                (setf (aref active-ids pos) last-id)
                                (setf (aref active-pos last-id) pos)
                                (decf active-count))
                              (setf rem-idx (aref z-next-rem rem-idx))))

                   ;; 2. Process Z-events: Add entering cuboids
                   (let ((add-idx (aref z-head-add z)))
                     (iterate (while (>= add-idx 0))
                              (setf (aref active-ids active-count) add-idx)
                              (setf (aref active-pos add-idx) active-count)
                              (incf active-count)
                              (setf add-idx (aref z-next-add add-idx))))

                   ;; 3. If there are active cuboids, perform 2D area union using X-sweep
                   (when (> active-count 0)
                     (let ((min-x max-x)
                           (mx -1)
                           (x-ev-count 0))
                       
                       ;; Distribute active cuboids into X-buckets
                       (iterate (for i from 0 below active-count)
                                (let* ((id (aref active-ids i))
                                       (x0 (aref cx0 id))
                                       (x1 (aref cx1 id))
                                       (y0 (aref cy0 id))
                                       (y1 (aref cy1 id)))
                                  (when (< x0 min-x) (setf min-x x0))
                                  (when (> x1 mx) (setf mx x1))

                                  ;; Left edge event (+1)
                                  (setf (aref x-next x-ev-count) (aref x-head x0))
                                  (setf (aref x-head x0) x-ev-count)
                                  (setf (aref x-is-add x-ev-count) 1)
                                  (setf (aref x-y0 x-ev-count) y0)
                                  (setf (aref x-y1 x-ev-count) y1)
                                  (incf x-ev-count)

                                  ;; Right edge event (-1)
                                  (setf (aref x-next x-ev-count) (aref x-head x1))
                                  (setf (aref x-head x1) x-ev-count)
                                  (setf (aref x-is-add x-ev-count) -1)
                                  (setf (aref x-y0 x-ev-count) y0)
                                  (setf (aref x-y1 x-ev-count) y1)
                                  (incf x-ev-count)))

                       ;; Sweep X-axis and update Segment Tree
                       (let ((current-area 0))
                         (iterate (for x from min-x to mx)
                                  (let ((idx (aref x-head x)))
                                    (when (>= idx 0)
                                      (iterate (while (>= idx 0))
                                               (update-tree 1 0 16384
                                                            (aref x-y0 idx)
                                                            (aref x-y1 idx)
                                                            (aref x-is-add idx))
                                               (setf idx (aref x-next idx)))
                                      ;; Reset bucket head to -1 cleanly for the next Z-slice
                                      (setf (aref x-head x) -1)))
                                  ;; Accumulate the Y-length coverage for the width of 1 unit
                                  (incf current-area (aref tree-length 1)))
                         ;; Add the 2D area (thickness is exactly 1 on Z-axis) to the total volume
                         (incf total-volume current-area)))))
          
          (format t "Done. Outputting result.~%")
          total-volume)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Step 1: Generating Fibonacci sequence...
Step 2: Building cuboids parameters...
Step 3: Initializing sweep line event buckets...
Step 4: Sweeping Z and X axis...
Done. Outputting result.

User time    =       51.720
System time  =        0.345
Elapsed time =       52.354
Allocation   = 10597672 bytes
2825 Page faults
GC time      =        0.004
 |------------------------------------------------------------|#
;;→ 328968937309
:ok