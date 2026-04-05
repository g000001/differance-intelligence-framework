;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0789 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0789)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)

(defconstant +p+ 2000000011)

;; -----------------------------------------------------------------------------
;; ユーティリティ: 64bit整数の算術
;; -----------------------------------------------------------------------------
(declaim (inline mod-inv))
(defun mod-inv (a m)
  (declare (type integer a m))
  (let ((m0 m)
        (y 0)
        (x 1)
        (a0 a))
    (declare (type integer m0 y x a0))
    (if (= m 1) (return-from mod-inv 0))
    (iterate (while (> a0 1))
      (let ((q (truncate a0 m0)))
        (let ((t-val m0))
          (setf m0 (rem a0 m0)
                a0 t-val)
          (setf t-val y
                y (- x (* q y))
                x t-val))))
    (if (< x 0) (+ x m) x)))

;; -----------------------------------------------------------------------------
;; データストレージ: フラット配列によるキャッシュ最適化
;; -----------------------------------------------------------------------------
(defvar *primes* (make-array 34 :element-type 'integer
                             :initial-contents '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 
                                                 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139)))

(defvar *a-mod-vals* (make-array 1000000 :element-type 'integer :adjustable t :fill-pointer 0))
(defvar *a-weights* (make-array 1000000 :element-type 'integer :adjustable t :fill-pointer 0))
(defvar *a-vals* (make-array 1000000 :element-type 'integer :adjustable t :fill-pointer 0))

(defvar *min-w* 1000000)
(defvar *best-product* 0)

(declaim (type integer *min-w*)
         (type integer *best-product*))

;; -----------------------------------------------------------------------------
;; DFS A: リストAの生成
;; -----------------------------------------------------------------------------
(defun dfs-a (idx w v m max-w)
  (declare (type integer idx w max-w)
           (type integer v m))
  (let* ((inv (mod-inv m +p+))
         (target (mod (- +p+ inv) +p+)))
    (vector-push-extend target *a-mod-vals*)
    (vector-push-extend w *a-weights*)
    (vector-push-extend v *a-vals*))

  (iterate (for i from idx below 34)
    (let* ((p (aref *primes* i))
           (pw (1- p)))
      (when (<= (+ w pw) max-w)
        (dfs-a i (+ w pw) (* v p) (mod (* m p) +p+) max-w)))))

;; -----------------------------------------------------------------------------
;; DFS B: 枝刈り付き探索
;; -----------------------------------------------------------------------------
(defun dfs-b (idx w v m max-w compressed-count)
  (declare (type integer idx w max-w)
           (type integer v m)
           (type fixnum compressed-count))
  
  ;; 二分探索
  (let ((left 0)
        (right (1- compressed-count)))
    (declare (type fixnum left right))
    (iterate (while (<= left right))
      (let* ((mid (ash (+ left right) -1))
             (mid-m (aref *a-mod-vals* mid)))
        (declare (type fixnum mid) (type integer mid-m))
        (cond ((= mid-m m)
               (let ((total-w (+ w (aref *a-weights* mid))))
                 (when (< total-w *min-w*)
                   (setf *min-w* total-w)
                   (setf *best-product* (* v (aref *a-vals* mid)))))
               (finish))
              ((< mid-m m)
               (setf left (1+ mid)))
              (t
               (setf right (1- mid)))))))

  ;; 動的枝刈り
  (iterate (for i from idx below 34)
    (let* ((p (aref *primes* i))
           (pw (1- p))
           (next-w (+ w pw)))
      (when (and (< next-w *min-w*) (<= next-w max-w))
        (dfs-b i next-w (* v p) (mod (* m p) +p+) max-w compressed-count)))))

;; -----------------------------------------------------------------------------
;; ソート用インデックス管理 (C版の Item 構造体ソートを模倣)
;; -----------------------------------------------------------------------------
(defun sort-list-a (count)
  (declare (type fixnum count))
  (let ((indices (make-array count :element-type 'fixnum)))
    (iterate (for i from 0 below count) (setf (aref indices i) i))
    ;; mod_val でソート
    (setf indices (sort indices #'(lambda (i j) (< (aref *a-mod-vals* i) (aref *a-mod-vals* j)))))
    
    ;; インデックスに基づいて配列を再配置
    (let ((new-mod (make-array count :element-type 'integer))
          (new-weight (make-array count :element-type 'integer))
          (new-val (make-array count :element-type 'integer)))
      (iterate (for i from 0 below count)
        (let ((old-idx (aref indices i)))
          (setf (aref new-mod i) (aref *a-mod-vals* old-idx))
          (setf (aref new-weight i) (aref *a-weights* old-idx))
          (setf (aref new-val i) (aref *a-vals* old-idx))))
      (setf (fill-pointer *a-mod-vals*) 0 (fill-pointer *a-weights*) 0 (fill-pointer *a-vals*) 0)
      (iterate (for i from 0 below count)
        (vector-push-extend (aref new-mod i) *a-mod-vals*)
        (vector-push-extend (aref new-weight i) *a-weights*)
        (vector-push-extend (aref new-val i) *a-vals*)))))

;; -----------------------------------------------------------------------------
;; メインエントリ
;; -----------------------------------------------------------------------------
(defun solve ()
  (setf (fill-pointer *a-mod-vals*) 0
        (fill-pointer *a-weights*) 0
        (fill-pointer *a-vals*) 0
        *min-w* 1000000
        *best-product* 0)

  (let ((w-a-max 110)
        (w-b-max 140))
    
    (format t "Phase 1: Generating List A...~%")
    (dfs-a 0 0 1 1 w-a-max)
    
    (let ((count (length *a-mod-vals*)))
      (format t "Sorting and Compressing ~A elements...~%" count)
      (sort-list-a count)

      ;; 重複削除 (圧縮)
      (let ((compressed-count 0)
            (prev-m #xFFFFFFFF))
        (declare (type fixnum compressed-count) (type integer prev-m))
        (iterate (for i from 0 below count)
          (let ((curr-m (aref *a-mod-vals* i)))
            (if (/= curr-m prev-m)
                (progn
                  (setf (aref *a-mod-vals* compressed-count) curr-m
                        (aref *a-weights* compressed-count) (aref *a-weights* i)
                        (aref *a-vals* compressed-count) (aref *a-vals* i))
                  (setf prev-m curr-m)
                  (incf compressed-count))
                (when (< (aref *a-weights* i) (aref *a-weights* (1- compressed-count)))
                  (setf (aref *a-weights* (1- compressed-count)) (aref *a-weights* i)
                        (aref *a-vals* (1- compressed-count)) (aref *a-vals* i))))))

        (format t "Phase 2: On-the-fly DFS with Dynamic Pruning...~%")
        (dfs-b 0 0 1 1 w-b-max compressed-count)))
    
    (format t "Final Answer: ~A~%" *best-product*)
    *best-product*))

#+| Do it | (solve )
