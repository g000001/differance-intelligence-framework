;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0244 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0244)

#||
(cl-text "Project Euler 244 Logic Projection"
  (cl-comment "1. Exact Integer Projection: States encoded structurally into bitmasks to avoid memory allocation.")
  (forall (state)
    (iff (ValidState state)
         (and (integerp state)
              (<= 0 (empty-pos state) 15)
              (= 7 (logcount (red-mask state))))))

  (cl-comment "2. Bijective Generation & Substructure: Merge symmetric states, linearly accumulating sums over DAG.")
  (forall (u v m)
    (if (and (Transition u v m) (= (dist v) (+ 1 (dist u))))
        (and (AccumulateWays v u)
             (AccumulateChecksum v u m))))
)
||#


(defun solve ()
  (let ((dist (make-array 1048576 :element-type '(unsigned-byte 8) :initial-element 255))
        (ways (make-array 1048576 :element-type '(unsigned-byte 32) :initial-element 0))
        (sum  (make-array 1048576 :element-type '(unsigned-byte 32) :initial-element 0))
        (q    (make-array 1048576 :element-type 'fixnum))
        (head 0)
        (tail 0)
        (+m+  100000007))
    (declare (type fixnum head tail +m+)
             (type (simple-array (unsigned-byte 8) (1048576)) dist)
             (type (simple-array (unsigned-byte 32) (1048576)) ways sum)
             (type (simple-array fixnum (1048576)) q))
    (let (;; 初期状態(S): E(0), R(1,4,5,8,9,12,13) -> 13106
          (start-state (logior (ash 0 16) 13106))
          ;; 目標状態(T): Checkerboard pattern -> R(2,5,7,8,10,13,15) -> 42404
          (target-state (logior (ash 0 16) 42404))
          (current-d 0))
      
      (setf (aref dist start-state) 0
            (aref ways start-state) 1
            (aref sum start-state) 0
            (aref q tail) start-state)
      (incf tail)

      (macrolet ((try-move (cond new-pos ascii)
                   `(when ,cond
                      (let* ((moved-pos ,new-pos)
                             (new-red red-mask))
                        ;; 動かすタイルが赤(1)であれば、そのビットを反転させ空白位置に赤を移動
                        (when (/= 0 (logand red-mask (ash 1 moved-pos)))
                          (setf new-red (logxor new-red (logior (ash 1 moved-pos) (ash 1 empty-pos)))))
                        (let* ((v (logior (ash moved-pos 16) new-red))
                               (vd (aref dist v)))
                          (cond
                            ((= vd 255)
                             (setf (aref dist v) (1+ d)
                                   (aref ways v) u-ways
                                   (aref sum v) (mod (+ (* u-sum 243) (* ,ascii u-ways)) +m+)
                                   (aref q tail) v)
                             (incf tail)
                             (when (>= tail 1048576)
                               (error "Queue Overflow: Non-Bijective Overcounting detected.")))
                            ((= vd (1+ d))
                             (setf (aref ways v) (mod (+ (aref ways v) u-ways) +m+)
                                   (aref sum v) (mod (+ (aref sum v) (* u-sum 243) (* ,ascii u-ways)) +m+)))))))))
        
        (iterate
          (while (< head tail))
          (for u = (aref q head))
          (for d = (aref dist u))
          (for u-ways = (aref ways u))
          (for u-sum = (aref sum u))
          (incf head)
          
          ;; 外周からの観測用プリントデバッグ
          (when (> d current-d)
            (format t "Exploring Depth ~A | Queue Head: ~A | Queue Tail: ~A~%" d head tail)
            (setf current-d d))
            
          ;; ターゲット状態以降の深度には潜らない（最短経路のみの合流を担保）
          (when (and (/= (aref dist target-state) 255)
                     (>= d (aref dist target-state)))
            (next-iteration))
          
          (for empty-pos = (ash u -16))
          (for red-mask = (logand u #xFFFF))
          (for x = (logand empty-pos 3))
          (for y = (ash empty-pos -2))
          
          ;; ASCII: L=76, R=82, U=85, D=68
          ;; 【注意】「タイルがスライドする方向」と「空白(Empty)が移動する方向」は逆である
          (try-move (< x 3) (1+ empty-pos) 76)  ;; Tile L -> Empty Right
          (try-move (> x 0) (1- empty-pos) 82)  ;; Tile R -> Empty Left
          (try-move (< y 3) (+ empty-pos 4) 85) ;; Tile U -> Empty Down
          (try-move (> y 0) (- empty-pos 4) 68) ;; Tile D -> Empty Up
          ))
      
      (format t "Target State Reached:~%")
      (format t "  Shortest Distance: ~A~%" (aref dist target-state))
      (format t "  Total Paths: ~A~%" (aref ways target-state))
      (format t "  Checksum Summation: ~A~%" (aref sum target-state))
      (aref sum target-state))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Exploring Depth 1 | Queue Head: 2 | Queue Tail: 3
Exploring Depth 2 | Queue Head: 4 | Queue Tail: 6
Exploring Depth 3 | Queue Head: 7 | Queue Tail: 11
Exploring Depth 4 | Queue Head: 12 | Queue Tail: 19
Exploring Depth 5 | Queue Head: 20 | Queue Tail: 34
Exploring Depth 6 | Queue Head: 35 | Queue Tail: 59
Exploring Depth 7 | Queue Head: 60 | Queue Tail: 96
Exploring Depth 8 | Queue Head: 97 | Queue Tail: 154
Exploring Depth 9 | Queue Head: 155 | Queue Tail: 247
Exploring Depth 10 | Queue Head: 248 | Queue Tail: 390
Exploring Depth 11 | Queue Head: 391 | Queue Tail: 602
Exploring Depth 12 | Queue Head: 603 | Queue Tail: 918
Exploring Depth 13 | Queue Head: 919 | Queue Tail: 1360
Exploring Depth 14 | Queue Head: 1361 | Queue Tail: 1955
Exploring Depth 15 | Queue Head: 1956 | Queue Tail: 2749
Exploring Depth 16 | Queue Head: 2750 | Queue Tail: 3807
Exploring Depth 17 | Queue Head: 3808 | Queue Tail: 5196
Exploring Depth 18 | Queue Head: 5197 | Queue Tail: 6979
Exploring Depth 19 | Queue Head: 6980 | Queue Tail: 9197
Exploring Depth 20 | Queue Head: 9198 | Queue Tail: 11881
Exploring Depth 21 | Queue Head: 11882 | Queue Tail: 15062
Exploring Depth 22 | Queue Head: 15063 | Queue Tail: 18789
Exploring Depth 23 | Queue Head: 18790 | Queue Tail: 23119
Exploring Depth 24 | Queue Head: 23120 | Queue Tail: 28032
Exploring Depth 25 | Queue Head: 28033 | Queue Tail: 33447
Exploring Depth 26 | Queue Head: 33448 | Queue Tail: 39263
Exploring Depth 27 | Queue Head: 39264 | Queue Tail: 45418
Exploring Depth 28 | Queue Head: 45419 | Queue Tail: 51803
Exploring Depth 29 | Queue Head: 51804 | Queue Tail: 58276
Exploring Depth 30 | Queue Head: 58277 | Queue Tail: 64626
Exploring Depth 31 | Queue Head: 64627 | Queue Tail: 70727
Exploring Depth 32 | Queue Head: 70728 | Queue Tail: 76465
Target State Reached:
  Shortest Distance: 32
  Total Paths: 1
  Checksum Summation: 96356848

User time    =        0.089
System time  =        0.013
Elapsed time =        0.071
Allocation   = 17869928 bytes
5289 Page faults
GC time      =        0.013
 |------------------------------------------------------------|#
;;→ 96356848
:ok

