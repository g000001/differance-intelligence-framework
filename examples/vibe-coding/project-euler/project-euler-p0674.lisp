;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0674 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0674)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defconstant $modulo #.(expt 10 9))
(defconstant $max-nodes #.(* 2 (expt 10 5)))

;; Global Graph Array
(defvar *g-type* (make-array $max-nodes :element-type 'fixnum))
(defvar *g-left* (make-array $max-nodes :element-type 'fixnum))
(defvar *g-right* (make-array $max-nodes :element-type 'fixnum))
(defvar *g-count* 0)

;; Unification & DFS State Arrays
(defvar *parent* (make-array $max-nodes :element-type 'fixnum))
(defvar *color* (make-array $max-nodes :element-type 'fixnum))
(defvar *value* (make-array $max-nodes :element-type 'fixnum))

;; Fast State Reset Arrays
(defvar *touched* (make-array 1000000 :element-type 'fixnum))
(defvar *touched-count* 0)

;; Unification Queue
(defvar *queue-l* (make-array $max-nodes :element-type 'fixnum))
(defvar *queue-r* (make-array $max-nodes :element-type 'fixnum))


(declaim (inline set-parent set-color calc-I))

(defun set-parent (x y)
  (declare (type fixnum x y))
  (when (= (aref *parent* x) x)
    (setf (aref *touched* *touched-count*) x)
    (incf *touched-count*))
  (setf (aref *parent* x) y))

(defun set-color (x c)
  (declare (type fixnum x c))
  (when (= (aref *color* x) 0)
    (setf (aref *touched* *touched-count*) x)
    (incf *touched-count*))
  (setf (aref *color* x) c))

(defun find-root (x)
  (declare (type fixnum x))
  (let ((p (aref *parent* x)))
    (if (= p x)
        x
        (let ((root (find-root p)))
          (when (/= p root)
            (set-parent x root))
          root))))

(defun calc-I (x y)
  (declare (type fixnum x y))
  (let* ((sum (mod (+ x y) $modulo))
         (sum1 (mod (+ sum 1) $modulo))
         (sum1-sq (mod (* sum1 sum1) $modulo)))
    (mod (+ sum1-sq y (- x) $modulo) $modulo)))

(defun reset-state ()
  (iter (for i from 0 below *touched-count*)
        (let ((idx (aref *touched* i)))
          (setf (aref *parent* idx) idx)
          (setf (aref *color* idx) 0)))
  (setf *touched-count* 0))

(defun init-global-state ()
  (iter (for i from 0 below *g-count*)
        (setf (aref *parent* i) i)
        (setf (aref *color* i) 0))
  (setf *touched-count* 0))

(defun parse-expr (str var-map)
  (let ((pos 0)
        (len (length str)))
    (labels ((peek () (if (< pos len) (char str pos) nil))
             (consume () (prog1 (peek) (incf pos)))
             (parse-I ()
               (consume) ;; 'I'
               (consume) ;; '('
               (let ((left (parse-term)))
                 (consume) ;; ','
                 (let ((right (parse-term)))
                   (consume) ;; ')'
                   (let ((id *g-count*))
                     (setf (aref *g-type* id) 1)
                     (setf (aref *g-left* id) left)
                     (setf (aref *g-right* id) right)
                     (incf *g-count*)
                     id))))
             (parse-term ()
               (if (char= (peek) #\I)
                   (parse-I)
                   (parse-var)))
             (parse-var ()
               (let ((start pos))
                 (iter (while (and (< pos len) (alpha-char-p (peek))))
                       (consume))
                 (let* ((name (intern (string-upcase (subseq str start pos)) :keyword))
                        (id (gethash name var-map)))
                   (unless id
                     (setf id *g-count*)
                     (setf (aref *g-type* id) 0)
                     (incf *g-count*)
                     (setf (gethash name var-map) id))
                   id))))
      (parse-term))))

(defun unify-roots (root1 root2)
  (declare (type fixnum root1 root2))
  (let ((head 0)
        (tail 0))
    (declare (type fixnum head tail))
    
    (setf (aref *queue-l* tail) root1)
    (setf (aref *queue-r* tail) root2)
    (incf tail)
    
    (iter (while (< head tail))
          (let* ((u (aref *queue-l* head))
                 (v (aref *queue-r* head)))
            (incf head)
            (let ((ru (find-root u))
                  (rv (find-root v)))
              (when (/= ru rv)
                (let ((tu (aref *g-type* ru))
                      (tv (aref *g-type* rv)))
                  (cond
                    ((and (= tu 1) (= tv 0))
                     (set-parent rv ru))
                    ((and (= tv 1) (= tu 0))
                     (set-parent ru rv))
                    ((and (= tu 1) (= tv 1))
                     (set-parent rv ru)
                     (setf (aref *queue-l* tail) (aref *g-left* ru))
                     (setf (aref *queue-r* tail) (aref *g-left* rv))
                     (incf tail)
                     (setf (aref *queue-l* tail) (aref *g-right* ru))
                     (setf (aref *queue-r* tail) (aref *g-right* rv))
                     (incf tail))
                    (t
                     (set-parent rv ru))))))))))

(defun evaluate-dag (root)
  (declare (type fixnum root))
  (labels ((dfs (node)
             (declare (type fixnum node))
             (let* ((r (find-root node))
                    (c (aref *color* r)))
               (cond
                 ((= c 1) -1) ;; Cycle Detected (Occurs Check Failed)
                 ((= c 2) (aref *value* r)) ;; Memoized DAG Eval
                 (t
                  (set-color r 1)
                  (let ((val 0))
                    (if (= (aref *g-type* r) 0)
                        (setf val 0) ;; 自由変数には最小化のため常に0を代入
                        (let ((val-l (dfs (aref *g-left* r))))
                          (if (= val-l -1)
                              (setf val -1)
                              (let ((val-r (dfs (aref *g-right* r))))
                                (if (= val-r -1)
                                    (setf val -1)
                                    (setf val (calc-I val-l val-r)))))))
                    (set-color r 2)
                    (setf (aref *value* r) val)
                    val))))))
    (dfs root)))

(defun solve ()
  (setf *g-count* 0)
  (let* ((candidates '("/tmp/0674_i_expressions.txt"))
         (var-map (make-hash-table :test #'eq))
         (expr-roots (make-array 500 :element-type 'fixnum :fill-pointer 0)))
    
    (iter (for path in candidates)
          (with-open-file (stream path :direction :input :if-does-not-exist nil)
            (when stream
              (format t "[DEBUG] Successfully loaded expressions from: ~A~%" path)
              (iter (for line = (read-line stream nil nil))
                    (while line)
                    (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
                      (when (plusp (length trimmed))
                        (vector-push (parse-expr trimmed var-map) expr-roots))))
              (return))))
              
    (let ((n (length expr-roots))
          (total-sum 0))
      (declare (type fixnum n total-sum))
      
      (init-global-state)
      
      (format t "[DEBUG] Total expressions: ~D~%" n)
      (format t "[DEBUG] Total graph nodes allocated: ~D~%" *g-count*)
      
      (iter (for i from 0 below n)
            (for r1 = (aref expr-roots i))
            (when (zerop (mod i 20))
              (format t "[DEBUG] Evaluating combination pairs starting with expression ~D...~%" i))
              
            (iter (for j from (1+ i) below n)
                  (for r2 = (aref expr-roots j))
                  
                  (reset-state)
                  (unify-roots r1 r2)
                  
                  (let ((val (evaluate-dag r1)))
                    (when (/= val -1)
                      (setf total-sum (mod (+ total-sum val) $modulo))))))
                      
      (format t "[DEBUG] Process completed smoothly. No memory bursts. Final Sum (mod 10^9): ~D~%" total-sum)
      total-sum)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[DEBUG] Successfully loaded expressions from: /tmp/0674_i_expressions.txt
[DEBUG] Total expressions: 149
[DEBUG] Total graph nodes allocated: 51619
[DEBUG] Evaluating combination pairs starting with expression 0...
[DEBUG] Evaluating combination pairs starting with expression 20...
[DEBUG] Evaluating combination pairs starting with expression 40...
[DEBUG] Evaluating combination pairs starting with expression 60...
[DEBUG] Evaluating combination pairs starting with expression 80...
[DEBUG] Evaluating combination pairs starting with expression 100...
[DEBUG] Evaluating combination pairs starting with expression 120...
[DEBUG] Evaluating combination pairs starting with expression 140...
[DEBUG] Process completed smoothly. No memory bursts. Final Sum (mod 10^9): 416678753

User time    =        1.336
System time  =        0.013
Elapsed time =        1.277
Allocation   = 4695336 bytes
435 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 416678753
:ok