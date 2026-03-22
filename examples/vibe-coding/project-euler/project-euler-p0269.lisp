;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0269 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0269)

;;; DPの状態 (a1, a2, a3) を 1D配列のインデックスにパックするマクロ
;;; a_k の理論上の絶対最大値は 81 であるため、[-100, 100] (201進数) にマッピングする
(defmacro pack (a1 a2 a3)
  `(+ (+ ,a1 100)
      (* (+ ,a2 100) 201)
      (* (+ ,a3 100) 40401)))

(defmacro unpack-a1 (val)
  `(- (mod ,val 201) 100))

(defmacro unpack-a2 (val)
  `(- (mod (truncate ,val 201) 201) 100))

(defmacro unpack-a3 (val)
  `(- (truncate ,val 40401) 100))

;;; 高速アクセスと O(1) クリアのためのステート管理構造体 (201^3 = 8120601要素)
(defstruct state-map
  (counts (make-array 8120601 :element-type '(unsigned-byte 64) :initial-element 0))
  (keys (make-array 200000 :element-type 'fixnum :fill-pointer 0 :adjustable nil)))

(defvar *cur-sm* nil)
(defvar *next-sm* nil)

(defun init-maps ()
  (unless *cur-sm* (setf *cur-sm* (make-state-map)))
  (unless *next-sm* (setf *next-sm* (make-state-map))))

(defun map-set (sm key val)
  (when (= (aref (state-map-counts sm) key) 0)
    (vector-push key (state-map-keys sm)))
  (incf (aref (state-map-counts sm) key) val))

(defun map-clear (sm)
  (iterate (for key in-vector (state-map-keys sm))
    (setf (aref (state-map-counts sm) key) 0))
  (setf (fill-pointer (state-map-keys sm)) 0))

;;; 根の集合 S から、対応する因子多項式 Q(x) = Π (x+r) を生成する
(defun poly-mul (p1 p2)
  (let ((res (make-array (+ (length p1) (length p2) -1) :initial-element 0)))
    (iterate (for i from 0 below (length p1))
      (iterate (for j from 0 below (length p2))
        (incf (aref res (+ i j)) (* (aref p1 i) (aref p2 j)))))
    res))

(defun get-q-list (S)
  (let ((res #(1)))
    (iterate (for r in S)
      (setf res (poly-mul res (vector r 1))))
    (coerce res 'list)))

;;; 根の積が 9 以下の全ての有効な部分集合を探索する
(defun get-valid-S ()
  (let ((res nil))
    (iterate (for i from 1 to 511)
      (let ((S nil)
            (prod 1))
        (iterate (for j from 0 to 8)
          (when (logbitp j i)
            (push (1+ j) S)
            (setf prod (* prod (1+ j)))))
        (when (<= prod 9)
          (push S res))))
    res))

;;; Q(x) で割り切れるか調べるDP
(defun solve-for-S (q-list L-max)
  (let* ((d (1- (length q-list)))
         (q0 (nth 0 q-list))
         (q1 (if (> (length q-list) 1) (nth 1 q-list) 0))
         (q2 (if (> (length q-list) 2) (nth 2 q-list) 0))
         (q3 (if (> (length q-list) 3) (nth 3 q-list) 0))
         (ans-array (make-array (1+ L-max) :initial-element 0 :element-type '(unsigned-byte 64))))
    
    (map-clear *cur-sm*)
    (map-set *cur-sm* (pack 0 0 0) 1)
    
    (iterate (for k from 0 below L-max)
      (map-clear *next-sm*)
      (iterate (for state in-vector (state-map-keys *cur-sm*))
        (let ((count (aref (state-map-counts *cur-sm*) state))
              (a1 (unpack-a1 state))
              (a2 (unpack-a2 state))
              (a3 (unpack-a3 state)))
          (iterate (for c from 0 to 9)
            ;; 定数項(k=0)が0のケースは別枠で一括計算するため弾く
            (unless (and (= k 0) (= c 0))
              (let ((v (- c (* q1 a1) (* q2 a2) (* q3 a3))))
                (when (= (mod v q0) 0)
                  (let ((a-new (truncate v q0)))
                    
                    ;; 数学的な安全網（これに引っかかることは証明上あり得ない）
                    (when (or (< a-new -100) (> a-new 100))
                      (error "a-new out of bounds: ~A. Requires buffer expansion." a-new))
                    
                    ;; 最上位桁は c>0 が保証されたもののみ加算 (高次項の余りが0になるか)
                    (when (> c 0)
                      (let ((valid (cond ((= d 1) (= a-new 0))
                                         ((= d 2) (and (= a-new 0) (= a1 0)))
                                         ((= d 3) (and (= a-new 0) (= a1 0) (= a2 0)))
                                         (t nil))))
                        (when valid
                          (incf (aref ans-array (1+ k)) count))))
                    
                    (map-set *next-sm* (pack a-new a1 a2) count))))))))
      
      (let ((tmp *cur-sm*))
        (setf *cur-sm* *next-sm*)
        (setf *next-sm* tmp)))
    ans-array))

;;; 包除原理 (Inclusion-Exclusion Principle) による各桁の集計
(defun calculate-YL (L-max)
  (init-maps)
  (let ((Y (make-array (1+ L-max) :initial-element 0 :element-type '(signed-byte 64)))
        (valid-S (get-valid-S)))
    (iterate (for S in valid-S)
      (let* ((q-list (get-q-list S))
             (ans (solve-for-S q-list L-max))
             (sign (if (oddp (length S)) 1 -1)))
        (iterate (for L from 1 to L-max)
          (incf (aref Y L) (* sign (aref ans L))))))
    Y))

(defun solve (&optional (target-n #.(expt 10 16)))
  (let* ((L-max (1- (length (princ-to-string target-n))))
         (Y (calculate-YL L-max))
         ;; ベースライン: 末尾が0の多項式 (P(0) = 0) は全ての根を無条件で満たす
         ;; 10^16までに末尾が0の数は 10^15個存在する
         (ans (expt 10 (1- L-max))))
    
    (format t "Calculating for Max Digits = ~A~%" L-max)
    
    ;; 数論的妥当性の自己チェックフェーズ: L=5 (Z(100,000))
    (let* ((Y-test (calculate-YL 5))
           (ans-test (expt 10 4)))
      (iterate (for L from 1 to 5)
        (incf ans-test (aref Y-test L)))
      (format t "Self-Correction Check: Z(10^5) evaluates to ~A (Expected: 14696)~%" ans-test))
      
    ;; メイン集計フェーズ
    (iterate (for L from 1 to L-max)
      (incf ans (aref Y L)))
    (format t "Final Answer Z(~A): ~A~%" target-n ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Calculating for Max Digits = 16
Self-Correction Check: Z(10^5) evaluates to 14696 (Expected: 14696)
Final Answer Z(10000000000000000): 1311109198529286

User time    =        0.140
System time  =        0.011
Elapsed time =        0.083
Allocation   = 4295157408 bytes
293 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 1311109198529286
:ok