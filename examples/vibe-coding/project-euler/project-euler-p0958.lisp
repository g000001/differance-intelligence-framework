;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0958 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0958)

(defparameter *f* (make-array 100 :element-type 'integer :initial-element 0))
(defparameter *best-m* 1000000000000000)
(defparameter *found* nil)

(defun init-fibs ()
  (setf (aref *f* 0) 0)
  (setf (aref *f* 1) 1)
  (iterate (for i from 2 to 99)
    (setf (aref *f* i) (+ (aref *f* (- i 1)) (aref *f* (- i 2))))))

(declaim (inline get-F))
(defun get-F (i)
  (declare (type fixnum i))
  (cond
    ((< i 0) 1)
    ((= i 0) 0)
    ((= i 1) 1)
    ((< i 100) (aref *f* i))
    (t 0)))

(declaim (inline lb))
(defun lb (u v n)
  (declare (type fixnum u v n))
  ;; 現在のコンティニュアント (u, v) から目標 N に到達するための最小必須コストを逆算
  ;; 1の連続がコンティニュアントを最大化するという数学的定理に基づく完璧な下界
  (iterate (for c from 0 to 90)
    (when (>= (+ (* u (get-F (1+ c))) (* v (get-F c))) n)
      (return c))))

(defun mod-inv (a m)
  (declare (type fixnum a m))
  (if (= m 1) 0
      (let ((t0 0) (t1 1) (r0 m) (r1 (mod a m)) (q 0))
        (declare (type fixnum t0 t1 r0 r1 q))
        (iterate (while (> r1 0))
          (setf q (floor r0 r1))
          (let ((temp-t (- t0 (* q t1)))
                (temp-r (- r0 (* q r1))))
            (setf t0 t1 t1 temp-t)
            (setf r0 r1 r1 temp-r)))
        (if (< t0 0) (+ t0 m) t0))))

(defun ida-star (u v cost limit n is-first)
  (declare (type fixnum u v cost limit n))
  ;; 目標の N に到達した場合
  (when (= u n)
    (setf *found* t)
    ;; 【数学的裏切りへの完全なカウンター (4-Orbit Recovery)】
    ;; コンティニュアント v は逆順連分数の分母 M_rev。
    ;; v^-1 mod N、およびそれらの補数から、対称性を持つ4つの軌道を生成し最小の M を抽出する。
    (let* ((m1 v)
           (m2 (- n v))
           (m3 (mod-inv v n))
           (m4 (- n m3)))
      (setf *best-m* (min *best-m* m1 m2 m3 m4)))
    (return-from ida-star nil))

  ;; 表現の一意性を保証するため、最初の部分商は 2 以上とする（m < n/2 の探索に限定）
  ;; 4-Orbit展開により、m > n/2 のケースも完全にカバーされるため数学的漏れはない
  (let ((start-q (if is-first 2 1)))
    (iterate (for q from start-q to n)
      (let ((next-u (+ (* q u) v)))
        (declare (type fixnum next-u))
        ;; 限界値を超過した瞬間に即座に打ち切るため、Bignumへの昇格すら発生しない
        (when (> next-u n) (return))
        
        (let ((next-cost (+ cost q)))
          (declare (type fixnum next-cost))
          ;; A* ヒューリスティック枝刈り: (現在のコスト + 到達への最小必須コスト) <= 制限値
          (when (<= (+ next-cost (lb next-u u n)) limit)
            (ida-star next-u u next-cost limit n nil)))))))

(defun solve (&optional (n (+ #.(expt 10 12) 39)))
  (init-fibs)
  (format t "Starting Ultimate Dimensional Collapse (IDA* & 4-Orbit Recovery) for N=~A~%" n)
  
  ;; Nに到達するための論理的最小ステップ数から反復深化を開始
  (let ((limit (lb 1 0 n)))
    (iterate
      (setf *best-m* n)
      (setf *found* nil)
      
      (ida-star 1 0 0 limit n t)
      
      ;; 最小コスト (limit) における全解の探索が完了した場合、結果を返す
      ;; これにより、等コストの中で確実に「最小の m」が選択される
      (when *found*
        (format t "Finished. Optimal subtraction steps ~A with minimal m = ~A~%" (1- limit) *best-m*)
        (return *best-m*))
      
      (incf limit))))


#+| Do it | (solve )