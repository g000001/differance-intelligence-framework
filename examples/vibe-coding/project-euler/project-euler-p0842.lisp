;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0842 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0842)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0))))))

(optimized-code-p T)



(defconstant +mod+ 1000000007)

(declaim (inline mod-add mod-sub mod-mul))
(defun mod-add (a b)
  (declare (type fixnum a b))
  (let ((sum (+ a b)))
    (if (>= sum +mod+) (- sum +mod+) sum)))

(defun mod-sub (a b)
  (declare (type fixnum a b))
  (let ((diff (- a b)))
    (if (< diff 0) (+ diff +mod+) diff)))

(defun mod-mul (a b)
  (declare (type fixnum a b))
  (mod (* a b) +mod+))

(defvar *fact* (make-array 65 :element-type 'fixnum :initial-element 1))
(defvar *inv-fact* (make-array 65 :element-type 'fixnum :initial-element 1))

(defun power (base exp)
  (declare (type fixnum base exp))
  (let ((res 1)
        (b base)
        (e exp))
    (declare (type fixnum res b e))
    (iterate (while (> e 0))
      (when (oddp e)
        (setf res (mod-mul res b)))
      (setf b (mod-mul b b))
      (setf e (ash e -1)))
    res))

(defun mod-inverse (n)
  (declare (type fixnum n))
  (power n (- +mod+ 2)))

(defun init-fact ()
  (iterate (for i from 1 to 64)
    (setf (aref *fact* i) (mod-mul (aref *fact* (1- i)) i)))
  (setf (aref *inv-fact* 64) (mod-inverse (aref *fact* 64)))
  (iterate (for i from 63 downto 0)
    (setf (aref *inv-fact* i) (mod-mul (aref *inv-fact* (1+ i)) (1+ i)))))

(defun mod-binom (n k)
  (declare (type fixnum n k))
  (if (or (< k 0) (> k n))
      0
      (mod-mul (aref *fact* n)
               (mod-mul (aref *inv-fact* k)
                        (aref *inv-fact* (- n k))))))

;;; 点Pにおける自己交差ハミルトン閉路数の計算式 C(k, n)
(defun C-val (k n)
  (declare (type fixnum k n))
  (let ((sum 0))
    (declare (type fixnum sum))
    (iterate (for i from 2 to k)
      (let* ((term (mod-mul (- i 1)
                            (mod-mul (mod-binom k i)
                                     (mod-mul (power 2 (- i 1))
                                              (aref *fact* (- n 1 i)))))))
        (if (evenp i)
            (setf sum (mod-add sum term))
            (setf sum (mod-sub sum term)))))
    sum))

;;; 重複カウント m から交差弦の数 k を逆算
(defun derive-k (m)
  (declare (type fixnum m))
  (let* ((D (+ 1 (* 8 m)))
         (s (isqrt D)))
    (if (= (* s s) D)
        (truncate (+ 1 s) 2)
        (error "Invalid intersection count m = ~A (Clustering split a mathematically perfect group)" m))))

(defun solve ()
  (init-fact)
  (let ((ans 0)
        ;; コンス生成(GC)を防ぐための 1D フラットバッファ
        (points-x (make-array 500000 :element-type 'double-float))
        (points-y (make-array 500000 :element-type 'double-float))
        (point-indices (make-array 500000 :element-type 'fixnum)))
    
    (iterate (for n from 3 to 60)
      (let ((idx 0)
            (pi/n (/ (coerce pi 'double-float) n)))
        (declare (type fixnum idx))
        
        ;; 全ての交点を列挙 (O(N^4))
        (iterate (for a from 0 below (- n 3))
          (iterate (for b from (+ a 1) below (- n 2))
            (iterate (for c from (+ b 1) below (- n 1))
              (iterate (for d from (+ c 1) below n)
                (let* ((ac+ (* (+ a c) pi/n))
                       (ac- (* (- a c) pi/n))
                       (bd+ (* (+ b d) pi/n))
                       (bd- (* (- b d) pi/n))
                       (c1 (cos ac+))
                       (s1 (sin ac+))
                       (r1 (cos ac-))
                       (c2 (cos bd+))
                       (s2 (sin bd+))
                       (r2 (cos bd-))
                       (delta (sin (- bd+ ac+))))
                  ;; 弦 AC と BD の交点座標
                  (setf (aref points-x idx) (/ (- (* r1 s2) (* s1 r2)) delta))
                  (setf (aref points-y idx) (/ (- (* c1 r2) (* r1 c2)) delta))
                  (setf (aref point-indices idx) idx)
                  (incf idx))))))
        
        (let ((total-n 0))
          (declare (type fixnum total-n))
          (when (> idx 0)
            ;; zero-allocationのための displaced array
            (let ((view (make-array idx :element-type 'fixnum :displaced-to point-indices)))
              ;; 浮動小数点ノイズに耐える堅牢なソート (Epsilon = 1e-9)
              (setf view (sort view (lambda (i j)
                                      (let* ((xi (aref points-x i))
                                             (xj (aref points-x j))
                                             (dx (- xi xj)))
                                        (if (< dx -1d-9) t
                                            (if (> dx 1d-9) nil
                                                (let* ((yi (aref points-y i))
                                                       (yj (aref points-y j))
                                                       (dy (- yi yj)))
                                                  (< dy -1d-9))))))))
              (let ((count 1)
                    (prev-idx (aref view 0)))
                (declare (type fixnum count prev-idx))
                (iterate (for i from 1 below idx)
                  (let* ((curr-idx (aref view i))
                         (dx (- (aref points-x curr-idx) (aref points-x prev-idx)))
                         (dy (- (aref points-y curr-idx) (aref points-y prev-idx)))
                         (dist-sq (+ (* dx dx) (* dy dy))))
                    ;; 距離の2乗が 1e-14 未満の場合は同一の交差点とみなす
                    (if (< dist-sq 1d-14)
                        (incf count)
                        (progn
                          (let ((k (derive-k count)))
                            (setf total-n (mod-add total-n (C-val k n))))
                          (setf count 1)
                          (setf prev-idx curr-idx)))))
                ;; 最後のグループ
                (let ((k (derive-k count)))
                  (setf total-n (mod-add total-n (C-val k n)))))))
          
          (format t "T(~A) = ~A~%" n total-n)
          (setf ans (mod-add ans total-n)))))
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
T(3) = 0
T(4) = 2
T(5) = 20
T(6) = 164
T(7) = 1680
T(8) = 14640
T(9) = 181440
T(10) = 1956864
T(11) = 26611200
T(12) = 320972160
T(13) = 189183965
T(14) = 901961715
T(15) = 674358851
T(16) = 265140830
T(17) = 663207224
T(18) = 763959211
T(19) = 76421738
T(20) = 966055586
T(21) = 109270953
T(22) = 20515495
T(23) = 434557593
T(24) = 840297354
T(25) = 808009378
T(26) = 489427299
T(27) = 788268426
T(28) = 835531204
T(29) = 413453887
T(30) = 800918563
T(31) = 243813160
T(32) = 837334399
T(33) = 142887670
T(34) = 377588396
T(35) = 705414522
T(36) = 379140881
T(37) = 837895278
T(38) = 595027650
T(39) = 334957618
T(40) = 347337136
T(41) = 959961330
T(42) = 973829647
T(43) = 515947208
T(44) = 411904655
T(45) = 654237928
T(46) = 149018619
T(47) = 770123318
T(48) = 622435080
T(49) = 117759951
T(50) = 12448618
T(51) = 996041344
T(52) = 193884704
T(53) = 802005056
T(54) = 958709156
T(55) = 833199638
T(56) = 15605189
T(57) = 787580726
T(58) = 853617648
T(59) = 623493122
T(60) = 441749095

User time    =        6.512
System time  =        0.079
Elapsed time =        6.526
Allocation   = 9634198296 bytes
5503 Page faults
GC time      =        0.075
 |------------------------------------------------------------|#
;;→ 885226002