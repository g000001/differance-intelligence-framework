;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0674 (:use cl series alexandria) (:export #:solve))
(in-package #:project-euler-0674)

(defconstant +max-nodes+ 200000)
(defparameter *is-app* (make-array +max-nodes+ :element-type 'boolean))
(defparameter *left-child* (make-array +max-nodes+ :element-type 'fixnum))
(defparameter *right-child* (make-array +max-nodes+ :element-type 'fixnum))
(defparameter *node-hash* (make-hash-table :test 'equal))
(defparameter *node-count* 0)

(defparameter *parent* (make-array +max-nodes+ :element-type 'fixnum))
(defparameter *state* (make-array +max-nodes+ :element-type 'fixnum))
(defparameter *memo* (make-array +max-nodes+ :element-type 'fixnum))

(defun reset-globals ()
  (clrhash *node-hash*)
  (setf *node-count* 0))

(defun get-var-node (name)
  "Retrieves or creates a variable node in the DAG."
  (prog ((id (gethash name *node-hash* 'not-found)))
    (when (not (eq id 'not-found))
      (return id))
    (setf id *node-count*)
    (incf *node-count*)
    (setf (aref *is-app* id) nil)
    (setf (gethash name *node-hash*) id)
    (return id)))

(defun get-app-node (L R)
  "Retrieves or creates an I-application node in the DAG."
  (prog ((key (cons L R))
         (id 0))
    (setf id (gethash key *node-hash* 'not-found))
    (when (not (eq id 'not-found))
      (return id))
    (setf id *node-count*)
    (incf *node-count*)
    (setf (aref *is-app* id) t)
    (setf (aref *left-child* id) L)
    (setf (aref *right-child* id) R)
    (setf (gethash key *node-hash*) id)
    (return id)))

(defun parse-to-node (str)
  "Parses an expression string into the global node arrays using flat state transitions."
  (prog ((s str)
         (inner "")
         (depth 0)
         (i 0)
         (len 0)
         (c #\space)
         (split-pos -1)
         (L 0)
         (R 0))
    (if (and (>= (length s) 2)
             (char= (char s 0) #\I)
             (char= (char s 1) #\())
        (go L-PARSE-COMPLEX)
        (return (get-var-node s)))

   L-PARSE-COMPLEX
    (setf inner (subseq s 2 (1- (length s))))
    (setf len (length inner))
   L-START
    (when (>= i len) (go L-END))
    (setf c (char inner i))
    (cond
      ((char= c #\() (incf depth))
      ((char= c #\)) (decf depth))
      ((and (char= c #\,) (= depth 0))
       (setf split-pos i)
       (go L-END)))
    (incf i)
    (go L-START)

   L-END
    (setf L (parse-to-node (subseq inner 0 split-pos)))
    (setf R (parse-to-node (subseq inner (1+ split-pos))))
    (return (get-app-node L R))))

(defun find-root (u)
  "Path compression Union-Find search."
  (prog ((root u) (curr u) (nxt 0))
   L-FIND
    (when (= (aref *parent* root) root) (go L-COMPRESS))
    (setf root (aref *parent* root))
    (go L-FIND)
   L-COMPRESS
    (when (= curr root) (return root))
    (setf nxt (aref *parent* curr))
    (setf (aref *parent* curr) root)
    (setf curr nxt)
    (go L-COMPRESS)))

(defun unify-nodes (u v)
  "Unifies two DAG nodes destructively in O(alpha(V)) time."
  (prog ((ru (find-root u))
         (rv (find-root v))
         (app-u nil)
         (app-v nil))
    (when (= ru rv) (return t))
    (setf app-u (aref *is-app* ru))
    (setf app-v (aref *is-app* rv))
    (cond
      ((not app-u)
       (setf (aref *parent* ru) rv)
       (return t))
      ((not app-v)
       (setf (aref *parent* rv) ru)
       (return t))
      (t
       ;; Both are applications. Union them and unify their children.
       (setf (aref *parent* ru) rv)
       (unify-nodes (aref *left-child* ru) (aref *left-child* rv))
       (unify-nodes (aref *right-child* ru) (aref *right-child* rv))
       (return t)))))

(defun eval-dag (u)
  "Evaluates the unified DAG with O(V) cycle detection (Occurs-Check replacement)."
  (prog ((ru (find-root u))
         (L-val 0)
         (R-val 0)
         (sum 0))
    ;; Cycle detection states: 0=unvisited, 1=visiting, 2=visited
    (when (= (aref *state* ru) 1) (return 'cycle))
    (when (= (aref *state* ru) 2) (return (aref *memo* ru)))
    
    (setf (aref *state* ru) 1)
    
    (when (not (aref *is-app* ru))
      (setf (aref *state* ru) 2)
      (setf (aref *memo* ru) 0)
      (return 0))
      
    (setf L-val (eval-dag (aref *left-child* ru)))
    (when (eq L-val 'cycle) (return 'cycle))
    
    (setf R-val (eval-dag (aref *right-child* ru)))
    (when (eq R-val 'cycle) (return 'cycle))
    
    (setf sum (mod (+ L-val R-val 1) 1000000000))
    (setf sum (mod (+ (mod (* sum sum) 1000000000) R-val (- L-val)) 1000000000))
    
    (setf (aref *state* ru) 2)
    (setf (aref *memo* ru) sum)
    (return sum)))

(defun init-pair ()
  "Resets the union-find and DAG states instantly."
  (prog ((i 0))
   L-INIT
    (when (>= i *node-count*) (return t))
    (setf (aref *parent* i) i)
    (setf (aref *state* i) 0)
    (incf i)
    (go L-INIT)))

(defun remove-duplicates-fixnum (lst)
  (prog ((res nil)
         (curr lst))
   L-LOOP
    (when (null curr) (return (nreverse res)))
    (when (not (member (car curr) res :test #'=))
      (push (car curr) res))
    (setf curr (cdr curr))
    (go L-LOOP)))

(defun remove-spaces (s)
  (remove-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) s))

(defun read-expressions-from-stream (stream)
  (prog ((lines nil)
         (line nil)
         (trimmed ""))
   L-READ
    (setf line (read-line stream nil))
    (when (null line)
      (return (nreverse lines)))
    (setf trimmed (remove-spaces line))
    (when (> (length trimmed) 0)
      (push (parse-to-node trimmed) lines))
    (go L-READ)))

(defun get-expressions ()
  "Locates and parses the PE target file gracefully."
  (prog ((paths '("/tmp/0674_i_expressions.txt"
                  "resources/documents/0674_i_expressions.txt"
                  "p674_i_expressions.txt"
                  "I-expressions.txt"
                  "../resources/documents/0674_i_expressions.txt"))
         (rest nil)
         (p nil)
         (stream nil)
         (exprs nil)
         (base-dir nil)
         (full-path nil))
    
    (setf base-dir (make-pathname :name nil :type nil 
                                  :defaults (or *load-truename* *compile-file-truename* *default-pathname-defaults*)))
    (setf rest paths)
   L-CHECK
    (when (null rest)
      (error "FATAL: Could not find the expressions file. Please ensure '0674_i_expressions.txt' exists in the working directory."))
    
    (setf p (car rest))
    (setf rest (cdr rest))
    
    (setf full-path (merge-pathnames p base-dir))
    (when (probe-file full-path)
      (setf stream (open full-path :direction :input))
      (setf exprs (read-expressions-from-stream stream))
      (close stream)
      (return (remove-duplicates-fixnum exprs)))
      
    (when (probe-file p)
      (setf stream (open p :direction :input))
      (setf exprs (read-expressions-from-stream stream))
      (close stream)
      (return (remove-duplicates-fixnum exprs)))
      
    (go L-CHECK)))

(defun solve-pairs (expr-nodes)
  "Calculates the O(N^2) pairing matrix via O(V alpha(V)) Union-Find in milliseconds."
  (prog ((n (length expr-nodes))
         (arr nil)
         (i 0)
         (j 0)
         (total-sum 0)
         (val 0))
    (when (= n 0) (return 0))
    (setf arr (make-array n :initial-contents expr-nodes :element-type 'fixnum))
   L-I
    (when (>= i n) (return total-sum))
    (setf j (1+ i))
   L-J
    (when (>= j n)
      (incf i)
      (go L-I))
    
    (init-pair)
    (unify-nodes (aref arr i) (aref arr j))
    (setf val (eval-dag (aref arr i)))
    (when (not (eq val 'cycle))
      (setf total-sum (mod (+ total-sum val) 1000000000)))
      
    (incf j)
    (go L-J)))

(defun solve ()
  (format t "--- Mathematical Grounding Validation ---~%")
  (reset-globals)
  (prog ((test-exprs nil)
         (test-ans 0)
         (exprs nil)
         (ans 0))
    (setf test-exprs (list (parse-to-node "I(x,I(z,t))")
                           (parse-to-node "I(I(y,z),y)")
                           (parse-to-node "I(I(x,z),y)")))
    (setf test-ans (solve-pairs test-exprs))
    (format t "Testing {A, B, C}... Expected: 26, Got: ~A~%" test-ans)
    (format t "-----------------------------------------~%")
    (format t "Solving for I-expressions.txt...~%")
    (reset-globals) ;; Cleans the slate before reading the large real dataset
    (setf exprs (get-expressions))
    (setf ans (solve-pairs exprs))
    (format t "Answer (last 9 digits): ~9,'0D~%" ans)
    (return ans)))


#+| Do it | (solve )