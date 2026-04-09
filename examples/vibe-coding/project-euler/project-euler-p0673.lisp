;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0673 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0673)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)


(defconstant +mod+ 999999937)

(defun mod-mul (a b)
  (declare (type fixnum a b))
  (mod (* a b) +mod+))

(defun mod-pow (base exp)
  (declare (type fixnum base exp))
  (let ((res 1)
        (b (mod base +mod+)))
    (declare (type fixnum res b))
    (iterate (while (> exp 0))
      (when (oddp exp)
        (setf res (mod-mul res b)))
      (setf b (mod-mul b b))
      (setf exp (ash exp -1)))
    res))

(defun mod-fact (n)
  (declare (type fixnum n))
  (let ((res 1))
    (declare (type fixnum res))
    (iterate (for i from 1 to n)
      (setf res (mod-mul res i)))
    res))

(defun load-pairs-file (filename n)
  (let ((arr (make-array (1+ n) :initial-element 0 :element-type 'fixnum)))
    (with-open-file (stream filename :direction :input)
      (iterate (for raw-line = (read-line stream nil nil))
               (while raw-line)
               (let ((line (string-trim '(#\Space #\Return #\Newline #\Tab) raw-line)))
                 (when (> (length line) 0)
                   (let ((comma-pos (position #\, line)))
                     (when comma-pos
                       (let ((u (parse-integer (subseq line 0 comma-pos)))
                             (v (parse-integer (subseq line (1+ comma-pos)))))
                         (setf (aref arr u) v)
                         (setf (aref arr v) u))))))))
    arr))

(defun find-file-or-error (name alt-name)
  (cond ((probe-file name) name)
        ((probe-file alt-name) alt-name)
        (t (error "Neither ~A nor ~A found in current directory." name alt-name))))

(defun solve (&optional (n 500))
  ;; ファイルの読み込み（ローカルにファイルが存在することを前提とする）
  (let* ((beds-file (find-file-or-error "/tmp/0673_beds.txt" "beds.txt"))
         (desks-file (find-file-or-error "/tmp/0673_desks.txt" "desks.txt"))
         (beds (load-pairs-file beds-file n))
         (desks (load-pairs-file desks-file n))
         (visited (make-array (1+ n) :initial-element nil))
         (shape-counts (make-hash-table :test 'equal)))
    
    ;; パス成分の抽出 (Pass 1: Paths)
    ;; 次数が2未満（端点）の頂点から探索を開始する
    (iterate (for i from 1 to n)
      (when (and (not (aref visited i))
                 (< (+ (if (> (aref beds i) 0) 1 0)
                       (if (> (aref desks i) 0) 1 0)) 2))
        (let ((curr i) (e1 0) (e2 0) (prev 0))
          (iterate
            (setf (aref visited curr) t)
            (let ((next-bed (aref beds curr))
                  (next-desk (aref desks curr)))
              (cond
                ((and (> next-bed 0) (not (= next-bed prev)) (not (aref visited next-bed)))
                 (incf e1)
                 (setf prev curr)
                 (setf curr next-bed))
                ((and (> next-desk 0) (not (= next-desk prev)) (not (aref visited next-desk)))
                 (incf e2)
                 (setf prev curr)
                 (setf curr next-desk))
                (t (return)))))
          ;; :path e1 e2 によって形状をユニークに分類
          (incf (gethash (list :path e1 e2) shape-counts 0)))))
    
    ;; サイクル成分の抽出 (Pass 2: Cycles)
    ;; 端点を持たない（すべての頂点が次数2の）未訪問成分を抽出する
    (iterate (for i from 1 to n)
      (when (not (aref visited i))
        (let ((curr i) (v-count 0))
          (iterate
            (setf (aref visited curr) t)
            (incf v-count)
            (let ((next-bed (aref beds curr))
                  (next-desk (aref desks curr)))
              (cond
                ((and (> next-bed 0) (not (aref visited next-bed)))
                 (setf curr next-bed))
                ((and (> next-desk 0) (not (aref visited next-desk)))
                 (setf curr next-desk))
                (t (return)))))
          ;; サイクルは頂点数 / 2 個のベッド辺と机辺を持つ
          (incf (gethash (list :cycle (/ v-count 2)) shape-counts 0)))))
    
    ;; 自己同型数に基づく順列の組み合わせの計算
    (let ((total-perms 1))
      (maphash (lambda (shape count)
                 (let ((M count)
                       (A (case (first shape)
                            ;; サイクルの自己同型数は 2k
                            (:cycle (* 2 (second shape)))
                            ;; パスは端点の色が同じ(e1 != e2)なら2、異なる(e1 == e2)なら1
                            (:path (if (= (second shape) (third shape)) 1 2)))))
                   (format t "Shape: ~A, Count: ~A, Auto: ~A~%" shape M A)
                   (setf total-perms (mod-mul total-perms (mod-fact M)))
                   (setf total-perms (mod-mul total-perms (mod-pow A M)))))
               shape-counts)
      
      (format t "Total permutations modulo ~D: ~D~%" +mod+ total-perms)
      total-perms)))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Shape: (path 6 6), Count: 5, Auto: 1
Shape: (path 2 2), Count: 3, Auto: 1
Shape: (path 1 0), Count: 2, Auto: 2
Shape: (path 0 0), Count: 2, Auto: 1
Shape: (path 8 9), Count: 8, Auto: 2
Shape: (path 15 14), Count: 7, Auto: 2
Shape: (cycle 4), Count: 5, Auto: 8
Shape: (cycle 2), Count: 1, Auto: 4
Shape: (cycle 3), Count: 2, Auto: 6
Shape: (path 0 1), Count: 2, Auto: 2
Total permutations modulo 999999937: 700325380

User time    =        0.001
System time  =        0.000
Elapsed time =        0.001
Allocation   = 342200 bytes
3 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 700325380
:ok