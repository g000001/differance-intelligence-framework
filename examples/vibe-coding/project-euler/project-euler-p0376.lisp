;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.5-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0376 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0376)


#||
(clif:property (NonTransitiveDice (CycleDirection)))
(clif:algorithm (DynamicProgramming (CustomLinearProbingHash)))
(clif:complexity (O (expt MaxStates 1)))
(clif:invariant (WinCountAccumulation (DeferredValueAssignment)))
||#

(declaim (inline pack unpack choose))

(defun pack (a b c wab wbc wca)
  "Packs 6 state variables into a single 27-bit Fixnum integer."
  (declare (type fixnum a b c wab wbc wca))
  (logior a
          (ash b 3)
          (ash c 6)
          (ash wab 9)
          (ash wbc 15)
          (ash wca 21)))

(defun unpack (packed)
  "Unpacks a 27-bit Fixnum back into 6 state variables."
  (declare (type fixnum packed))
  (values (logand packed 7)
          (logand (ash packed -3) 7)
          (logand (ash packed -6) 7)
          (logand (ash packed -9) 63)
          (logand (ash packed -15) 63)
          (logand (ash packed -21) 63)))

(defun choose (n k)
  "Calculates the binomial coefficient (n choose k)."
  (declare (type fixnum n k))
  (if (or (< k 0) (> k n))
      0
      (let ((res 1))
        (declare (type integer res))
        (iterate (for i from 1 to k)
          (setf res (/ (* res (+ (- n i) 1)) i)))
        res)))

(defun solve (&optional (n 30))
  ;; Use a custom flat-array hash table (Linear Probing) to completely avoid Lisp's GC overhead
  (let* ((cap-shift 23) ; 8,388,608 elements (sufficient for the ~3 million max states)
         (capacity (ash 1 cap-shift))
         (mask (1- capacity))
         (keys-dp (make-array capacity :element-type 'fixnum :initial-element -1))
         (vals-dp (make-array capacity :element-type '(unsigned-byte 62) :initial-element 0))
         (act-dp (make-array capacity :element-type 'fixnum :fill-pointer 0))
         (keys-next (make-array capacity :element-type 'fixnum :initial-element -1))
         (vals-next (make-array capacity :element-type '(unsigned-byte 62) :initial-element 0))
         (act-next (make-array capacity :element-type 'fixnum :fill-pointer 0))
         (ans 0))
    (declare (type (simple-array fixnum (*)) keys-dp keys-next)
             (type (simple-array (unsigned-byte 62) (*)) vals-dp vals-next)
             (type (array fixnum (*)) act-dp act-next)
             (type integer ans))

    (labels ((hash-key (k)
               (declare (type fixnum k))
               ;; A simple multiplicative hash to spread the 27-bit keys
               (logand (ash (* k 26544357) -8) mask))
             (put-next (k v)
               (declare (type fixnum k)
                        (type (unsigned-byte 62) v))
               (let ((idx (hash-key k)))
                 (declare (type fixnum idx))
                 (iterate
                   (let ((ck (aref keys-next idx)))
                     (declare (type fixnum ck))
                     (cond ((= ck k)
                            (setf (aref vals-next idx) (+ (aref vals-next idx) v))
                            (return))
                           ((= ck -1)
                            (setf (aref keys-next idx) k)
                            (setf (aref vals-next idx) v)
                            (vector-push idx act-next)
                            (return))
                           (t
                            (setf idx (logand (1+ idx) mask))))))))
             (clear-next ()
               (iterate (for i from 0 below (length act-next))
                 (setf (aref keys-next (aref act-next i)) -1))
               (setf (fill-pointer act-next) 0)))

      ;; Initial state: 0 faces assigned for all dice, 0 win counts
      (let ((init-state (pack 0 0 0 0 0 0)))
        (setf (aref keys-dp (hash-key init-state)) init-state)
        (setf (aref vals-dp (hash-key init-state)) 1)
        (vector-push (hash-key init-state) act-dp))

      (format t "Calculating non-transitive dice sets for N=~A...~%" n)

      ;; A maximum of 18 distinct values can be used across all 3 dice (6 faces * 3)
      (iterate (for k from 1 to 18)
        (clear-next)
        (let ((ways (choose n k)))
          (declare (type integer ways))
          
          (iterate (for i from 0 below (length act-dp))
            (let* ((idx (aref act-dp i))
                   (state (aref keys-dp idx))
                   (count (aref vals-dp idx)))
              (declare (type fixnum idx state)
                       (type (unsigned-byte 62) count))
              
              (multiple-value-bind (a b c wab wbc wca) (unpack state)
                (declare (type fixnum a b c wab wbc wca))
                
                (let ((rem-a (- 6 a))
                      (rem-b (- 6 b))
                      (rem-c (- 6 c)))
                  (declare (type fixnum rem-a rem-b rem-c))
                  
                  ;; Try all valid combinations of adding new faces with the current value
                  (iterate (for da from 0 to rem-a)
                    (declare (type fixnum da))
                    (iterate (for db from 0 to rem-b)
                      (declare (type fixnum db))
                      (iterate (for dc from 0 to rem-c)
                        (declare (type fixnum dc))
                        (when (> (+ da db dc) 0)
                          (let ((na (+ a da))
                                (nb (+ b db))
                                (nc (+ c dc))
                                ;; A's new faces beat all of B's old faces
                                (nwab (+ wab (* da b)))
                                (nwbc (+ wbc (* db c)))
                                (nwca (+ wca (* dc a))))
                            (declare (type fixnum na nb nc nwab nwbc nwca))
                            
                            (if (and (= na 6) (= nb 6) (= nc 6))
                                ;; If all dice are fully assigned, check nontransitive condition
                                (when (and (> nwab 18) (> nwbc 18) (> nwca 18))
                                  (incf ans (* count ways)))
                                ;; Otherwise, push to the next step
                                (let ((nstate (pack na nb nc nwab nwbc nwca)))
                                  (declare (type fixnum nstate))
                                  (put-next nstate count)))))))))))))
        
        ;; Swap double buffers
        (rotatef keys-dp keys-next)
        (rotatef vals-dp vals-next)
        (rotatef act-dp act-next)
        (format t "k=~A, active states=~A~%" k (length act-dp)))

      ;; Directed cycle A -> B -> C -> A captures half of all cyclic sets.
      ;; Each unique set has exactly 3 such permutations, so we divide by 3.
      (let ((final-ans (/ ans 3)))
        (format t "Final ans=~A~%" final-ans)
        final-ans))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating non-transitive dice sets for N=30...
k=1, active states=341
k=2, active states=16974
k=3, active states=170300
k=4, active states=387865
k=5, active states=504008
k=6, active states=527650
k=7, active states=517700
k=8, active states=498238
k=9, active states=477808
k=10, active states=456324
k=11, active states=430929
k=12, active states=397383
k=13, active states=351354
k=14, active states=289818
k=15, active states=214371
k=16, active states=132510
k=17, active states=56553
k=18, active states=0
Final ans=973059630185670

User time    =       12.796
System time  =        0.166
Elapsed time =       12.882
Allocation   = 402931256 bytes
67880 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 973059630185670
:ok