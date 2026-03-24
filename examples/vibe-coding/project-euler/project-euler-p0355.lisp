;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0355 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0355)

#||
(cl:comment "PE 355 Mathematical Constraints and Shortcuts")
(cl:comment "Constraint 1: The composite numbers forming the maximum sum must exclusively take the form of q^k * p, where q is a small prime (<= sqrt(N)) and p is a large prime (> sqrt(N)).")
(cl:comment "Constraint 2: Each small prime can participate in at most one such composite number. The same applies to each large prime.")
(cl:comment "Shortcut: This structure maps perfectly to Maximum Weight Bipartite Matching.")
(cl:comment "Resolution: Abandoning exponential DFS, we model the bipartite graph as a Flow Network and solve it in polynomial time using Minimum Cost Maximum Flow (MCMF).")
||#


(defstruct (directed-edge (:type vector))
  (destination-node 0 :type fixnum)
  (residual-capacity 0 :type fixnum)
  (edge-cost 0 :type fixnum)
  (reverse-edge-index 0 :type fixnum))

(defun minimum-cost-maximum-flow (source-node sink-node total-vertices flow-network-graph)
  (let ((total-minimum-cost 0)
        (infinity-value (expt 10 15))
        (shortest-path-distance-array (make-array total-vertices :element-type 'integer :initial-element 0))
        (previous-vertex-array (make-array total-vertices :element-type 'fixnum :initial-element 0))
        (previous-edge-index-array (make-array total-vertices :element-type 'fixnum :initial-element 0))
        (vertex-in-queue-bit-vector (make-array total-vertices :element-type 'bit :initial-element 0))
        ;; Ring buffer or sufficiently large queue for SPFA
        (spfa-queue (make-array (* total-vertices 10) :element-type 'fixnum :initial-element 0)))
    
    (iterate (while t)
      (fill shortest-path-distance-array infinity-value)
      (fill previous-vertex-array -1)
      (fill previous-edge-index-array -1)
      (fill vertex-in-queue-bit-vector 0)
      
      (setf (aref shortest-path-distance-array source-node) 0)
      (setf (aref spfa-queue 0) source-node)
      (setf (sbit vertex-in-queue-bit-vector source-node) 1)
      
      (let ((queue-head-index 0)
            (queue-tail-index 1))
        (iterate (while (< queue-head-index queue-tail-index))
          (let ((current-vertex (aref spfa-queue queue-head-index)))
            (incf queue-head-index)
            (setf (sbit vertex-in-queue-bit-vector current-vertex) 0)
            
            (let ((adjacent-edges (aref flow-network-graph current-vertex)))
              (iterate (for edge-index from 0 below (length adjacent-edges))
                (let* ((edge (aref adjacent-edges edge-index))
                       (destination (directed-edge-destination-node edge))
                       (capacity (directed-edge-residual-capacity edge))
                       (cost (directed-edge-edge-cost edge)))
                  (when (and (> capacity 0)
                             (< (+ (aref shortest-path-distance-array current-vertex) cost)
                                (aref shortest-path-distance-array destination)))
                    (setf (aref shortest-path-distance-array destination)
                          (+ (aref shortest-path-distance-array current-vertex) cost))
                    (setf (aref previous-vertex-array destination) current-vertex)
                    (setf (aref previous-edge-index-array destination) edge-index)
                    (when (zerop (sbit vertex-in-queue-bit-vector destination))
                      (setf (aref spfa-queue queue-tail-index) destination)
                      (incf queue-tail-index)
                      (setf (sbit vertex-in-queue-bit-vector destination) 1)))))))))
                      
      (when (= (aref shortest-path-distance-array sink-node) infinity-value)
        (return-from minimum-cost-maximum-flow total-minimum-cost))
        
      (let ((shortest-distance-to-sink (aref shortest-path-distance-array sink-node)))
        ;; If the shortest path cost is non-negative, further flow will only decrease the maximum gain.
        (if (>= shortest-distance-to-sink 0)
            (return-from minimum-cost-maximum-flow total-minimum-cost))
            
        ;; Push exactly 1 unit of flow since bipartite capacities are strictly 1
        (incf total-minimum-cost shortest-distance-to-sink)
        
        (let ((traceback-vertex sink-node))
          (iterate (while (/= traceback-vertex source-node))
            (let* ((prev-v (aref previous-vertex-array traceback-vertex))
                   (prev-e-idx (aref previous-edge-index-array traceback-vertex))
                   (forward-edge (aref (aref flow-network-graph prev-v) prev-e-idx))
                   (rev-idx (directed-edge-reverse-edge-index forward-edge))
                   (backward-edge (aref (aref flow-network-graph traceback-vertex) rev-idx)))
              (decf (directed-edge-residual-capacity forward-edge) 1)
              (incf (directed-edge-residual-capacity backward-edge) 1)
              (setq traceback-vertex prev-v))))))))

(defun solve ()
  (let* ((limit-number 200000)
         (is-composite-bit-vector (make-array (1+ limit-number) :element-type 'bit :initial-element 0))
         (prime-numbers-array (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t))
         (maximum-prime-power-array (make-array (1+ limit-number) :element-type 'integer :initial-element 0))
         (baseline-sum-of-powers 1)) ; 1 is mutually coprime with everything
    
    (format t "Sieving primes up to ~A...~%" limit-number)
    (iterate (for i from 2 to limit-number)
      (when (zerop (sbit is-composite-bit-vector i))
        (vector-push-extend i prime-numbers-array)
        (iterate (for j from (* i i) to limit-number by i)
          (setf (sbit is-composite-bit-vector j) 1))))
          
    (iterate (for p in-vector prime-numbers-array)
      (let ((max-power p))
        (iterate (while (<= (* max-power p) limit-number))
          (setq max-power (* max-power p)))
        (setf (aref maximum-prime-power-array p) max-power)
        (incf baseline-sum-of-powers max-power)))
        
    (format t "Baseline sum of maximal prime powers: ~A~%" baseline-sum-of-powers)
        
    (let ((small-primes-array (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t))
          (large-primes-array (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t)))
      (iterate (for p in-vector prime-numbers-array)
        (if (<= p 447)
            (vector-push-extend p small-primes-array)
            (vector-push-extend p large-primes-array)))
            
      (let* ((num-small-primes (length small-primes-array))
             (num-large-primes (length large-primes-array))
             (total-vertices (+ 2 num-small-primes num-large-primes))
             (source-node 0)
             (sink-node 1)
             (flow-network-graph (make-array total-vertices)))
             
        (iterate (for i from 0 below total-vertices)
          (setf (aref flow-network-graph i) (make-array 0 :element-type 'vector :fill-pointer 0 :adjustable t)))
          
        (labels ((add-network-edge (from to cap cost)
                   (let ((from-edges (aref flow-network-graph from))
                         (to-edges (aref flow-network-graph to)))
                     (let ((from-idx (length from-edges))
                           (to-idx (length to-edges)))
                       (vector-push-extend (make-directed-edge :destination-node to :residual-capacity cap :edge-cost cost :reverse-edge-index to-idx) from-edges)
                       (vector-push-extend (make-directed-edge :destination-node from :residual-capacity 0 :edge-cost (- cost) :reverse-edge-index from-idx) to-edges)))))
          
          (format t "Constructing Minimum Cost Flow Network...~%")
          ;; 1. Connect Source to all Small Primes (Capacity 1, Cost 0)
          (iterate (for i from 0 below num-small-primes)
            (add-network-edge source-node (+ i 2) 1 0))
            
          ;; 2. Connect all Large Primes to Sink (Capacity 1, Cost 0)
          (iterate (for j from 0 below num-large-primes)
            (add-network-edge (+ num-small-primes j 2) sink-node 1 0))
            
          ;; 3. Connect Bipartite Edges where gain > 0
          (iterate (for i from 0 below num-small-primes)
            (let ((q (aref small-primes-array i))
                  (small-node-id (+ i 2)))
              (iterate (for j from 0 below num-large-primes)
                (let ((p (aref large-primes-array j))
                      (large-node-id (+ num-small-primes j 2))
                      (maximum-weight 0))
                  (iterate (for k from 1)
                    (let ((qk (expt q k)))
                      (if (> (* qk p) limit-number)
                          (finish)
                          (let ((weight (- (* qk p) (aref maximum-prime-power-array q) p)))
                            (when (> weight maximum-weight)
                              (setf maximum-weight weight))))))
                  (when (> maximum-weight 0)
                    ;; Cost is inverted because we want Maximum Weight (Minimum Negative Cost)
                    (add-network-edge small-node-id large-node-id 1 (- maximum-weight)))))))
                    
          (format t "Graph Built. Executing SPFA Max-Flow Min-Cost...~%")
          (let* ((minimum-cost (minimum-cost-maximum-flow source-node sink-node total-vertices flow-network-graph))
                 (optimal-gain (- minimum-cost)) ; Invert back to positive gain
                 (final-answer (+ baseline-sum-of-powers optimal-gain)))
                 
            (format t "Optimal additional gain achieved via Network Flow: ~A~%" optimal-gain)
            (format t "Final Answer Co(~A): ~A~%" limit-number final-answer)
            final-answer))))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Sieving primes up to 200000...
Baseline sum of maximal prime powers: 1716018696
Constructing Minimum Cost Flow Network...
Graph Built. Executing SPFA Max-Flow Min-Cost...
Optimal additional gain achieved via Network Flow: 10526311
Final Answer Co(200000): 1726545007

User time    =        0.516
System time  =        0.021
Elapsed time =        0.480
Allocation   = 4307788344 bytes
5165 Page faults
GC time      =        0.008
 |------------------------------------------------------------|#
;;→ 1726545007
:ok