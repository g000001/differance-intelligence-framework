;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0253 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0253)

(defun insert-desc (x lst)
  "降順を維持してリストに要素を挿入する"
  (cond ((null lst) (list x))
        ((>= x (car lst)) (cons x lst))
        (t (cons (car lst) (insert-desc x (cdr lst))))))

(defun remove-one (x lst)
  "リストから要素 x を1つだけ削除する"
  (cond ((null lst) nil)
        ((= x (car lst)) (cdr lst))
        (t (cons (car lst) (remove-one x (cdr lst))))))

(defmacro add-to-dp (dp m e2 e1 inners w)
  "DPテーブルへの安全な重み加算マクロ"
  `(let* ((key (list ,m ,e2 ,e1 ,inners))
          (val (gethash key ,dp 0)))
     (setf (gethash key ,dp) (+ val ,w))))

(defun round-to-6-decimal (ratio)
  "有理数(Rational)から完全な精度で10進6桁の四捨五入を行う"
  (let* ((scaled (+ (* ratio 1000000) 1/2))
         (int-val (floor scaled)))
    (format nil "~D.~6,'0D" (truncate int-val 1000000) (mod int-val 1000000))))

(defun solve (&optional (n 40))
  (format t "Starting perfect topological DP for N = ~A...~%" n)
  (let ((current-dp (make-hash-table :test 'equal)))
    ;; 初期状態: M=0, Ends2=N, Ends1=(), Inners=() -> 重み 1
    (setf (gethash (list 0 n nil nil) current-dp) 1)
    
    (iterate (for t-step from 0 below n)
      (let ((next-dp (make-hash-table :test 'equal)))
        (maphash 
         (lambda (key w)
           (destructuring-bind (m e2 e1 inners) key
             
             ;; 1. Ends2 (両端壁) からの遷移
             (when e2
               (let ((L e2))
                 (if (= L 1)
                     (let* ((new-c (1+ (length inners)))
                            (new-m (max m new-c)))
                       (add-to-dp next-dp new-m nil nil inners w))
                     (progn
                       ;; 端に置く (2通り) -> Ends1(L-1) が生成
                       (let* ((new-L (- L 1))
                              (new-e1 (if (> new-L 0) (list new-L) nil))
                              (new-c (1+ (length inners)))
                              (new-m (max m new-c)))
                         (add-to-dp next-dp new-m nil new-e1 inners (* w 2)))
                       ;; 内部に置く -> Ends1(k) と Ends1(L-1-k) に分断
                       ;; 【修正点】上限は (L-1)/2 でなければ奇数の対称分割が欠落する
                       (iterate (for k from 1 to (floor (- L 1) 2))
                         (let* ((j (- L 1 k))
                                (new-e1 (if (= k j) (list k k) (list j k))) 
                                (new-c (1+ (length inners)))
                                (new-m (max m new-c))
                                (weight (if (= k j) 1 2)))
                           (add-to-dp next-dp new-m nil new-e1 inners (* w weight))))))))
                     
             ;; 2. Ends1 (片端壁・片端ピース) からの遷移
             (let ((seen nil))
               (dolist (L e1)
                 (unless (member L seen)
                   (push L seen)
                   (let* ((count-L (funcall #'count L e1))
                          (base-w (* w count-L))
                          (rest-e1 (remove-one L e1)))
                     (if (= L 1)
                         ;; L=1 の場合: ギャップ消滅 (1通り)
                         (let* ((new-c (1+ (length inners)))
                                (new-m (max m new-c)))
                           (add-to-dp next-dp new-m e2 rest-e1 inners base-w))
                         ;; L >= 2 の場合
                         (progn
                           ;; 壁際に置く -> Inners(L-1) に昇格
                           (let* ((new-inners (insert-desc (- L 1) inners))
                                  (new-c (1+ (length new-inners)))
                                  (new-m (max m new-c)))
                             (add-to-dp next-dp new-m e2 rest-e1 new-inners base-w))
                           ;; ピース際に置く -> Ends1(L-1) が維持
                           (let* ((new-e1 (insert-desc (- L 1) rest-e1))
                                  (new-c (1+ (length inners)))
                                  (new-m (max m new-c)))
                             (add-to-dp next-dp new-m e2 new-e1 inners base-w))
                           ;; 内部に置く -> Ends1(k) と Inners(L-1-k) に分断
                           ;; 非対称なので 1 から L-2 まで全走査
                           (iterate (for k from 1 to (- L 2))
                             (let* ((end-len k)
                                    (inner-len (- L 1 k))
                                    (new-e1 (insert-desc end-len rest-e1))
                                    (new-inners (insert-desc inner-len inners))
                                    (new-c (1+ (length new-inners)))
                                    (new-m (max m new-c)))
                               (add-to-dp next-dp new-m e2 new-e1 new-inners base-w)))))))))
                         
             ;; 3. Inners (両端ピース) からの遷移
             (let ((seen nil))
               (dolist (L inners)
                 (unless (member L seen)
                   (push L seen)
                   (let* ((count-L (funcall #'count L inners))
                          (base-w (* w count-L))
                          (rest-inners (remove-one L inners)))
                     (if (= L 1)
                         ;; L=1 の場合: ギャップ消滅し2つの島が連結 (1通り)
                         (let* ((new-c (1+ (length rest-inners)))
                                (new-m (max m new-c)))
                           (add-to-dp next-dp new-m e2 e1 rest-inners base-w))
                         ;; L >= 2 の場合
                         (progn
                           ;; ピース際(端)に置く (2通り) -> Inners(L-1) が維持
                           (let* ((new-inners (insert-desc (- L 1) rest-inners))
                                  (new-c (1+ (length new-inners)))
                                  (new-m (max m new-c)))
                             (add-to-dp next-dp new-m e2 e1 new-inners (* base-w 2)))
                           ;; 内部に置く -> Inners(k) と Inners(L-1-k) に分断
                           ;; 【修正点】上限は (L-1)/2 でなければ奇数の対称分割が欠落する
                           (iterate (for k from 1 to (floor (- L 1) 2))
                             (let* ((j (- L 1 k))
                                    (new-inners-1 (insert-desc j rest-inners))
                                    (new-inners-2 (insert-desc k new-inners-1)) ; 降順ソート維持
                                    (new-c (1+ (length new-inners-2)))
                                    (new-m (max m new-c))
                                    (weight (if (= k j) 1 2)))
                               (add-to-dp next-dp new-m e2 e1 new-inners-2 (* base-w weight))))))))))))
         current-dp)
        
        ;; 検証: 総重みは厳密に階乗で増加しなければならない
        (let ((step-total 0))
          (maphash (lambda (k w) (declare (ignore k)) (incf step-total w)) next-dp)
          (format t "Pieces placed: ~2D, Active States: ~5D, Total W: ~A~%" 
                  (1+ t-step) (hash-table-count next-dp) step-total))
        (setf current-dp next-dp)))
        
    (format t "Aggregating expectations...~%")
    (let ((total-w 0)
          (expected-sum 0))
      (maphash (lambda (key w)
                 (let ((m (first key)))
                   (incf total-w w)
                   (incf expected-sum (* m w))))
               current-dp)
      (format t "Total valid permutations: ~A~%" total-w)
      (format t "Expected sum as Rational: ~A~%" (/ expected-sum total-w))
      (round-to-6-decimal (/ expected-sum total-w)))))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting perfect topological DP for N = 40...
Pieces placed:  1, Active States:    20, Total W: 40
Pieces placed:  2, Active States:   400, Total W: 1560
Pieces placed:  3, Active States:  2679, Total W: 59280
Pieces placed:  4, Active States:  9650, Total W: 2193360
Pieces placed:  5, Active States: 23301, Total W: 78960960
Pieces placed:  6, Active States: 43020, Total W: 2763633600
Pieces placed:  7, Active States: 65630, Total W: 93963542400
Pieces placed:  8, Active States: 87307, Total W: 3100796899200
Pieces placed:  9, Active States: 104767, Total W: 99225500774400
Pieces placed: 10, Active States: 116420, Total W: 3075990524006400
Pieces placed: 11, Active States: 121735, Total W: 92279715720192000
Pieces placed: 12, Active States: 121510, Total W: 2676111755885568000
Pieces placed: 13, Active States: 116656, Total W: 74931129164795904000
Pieces placed: 14, Active States: 108664, Total W: 2023140487449489408000
Pieces placed: 15, Active States: 98514, Total W: 52601652673686724608000
Pieces placed: 16, Active States: 87458, Total W: 1315041316842168115200000
Pieces placed: 17, Active States: 76055, Total W: 31560991604212034764800000
Pieces placed: 18, Active States: 65118, Total W: 725902806896876799590400000
Pieces placed: 19, Active States: 54801, Total W: 15969861751731289590988800000
Pieces placed: 20, Active States: 45553, Total W: 335367096786357081410764800000
Pieces placed: 21, Active States: 37272, Total W: 6707341935727141628215296000000
Pieces placed: 22, Active States: 30188, Total W: 127439496778815690936090624000000
Pieces placed: 23, Active States: 24064, Total W: 2293910942018682436849631232000000
Pieces placed: 24, Active States: 19014, Total W: 38996486014317601426443730944000000
Pieces placed: 25, Active States: 14767, Total W: 623943776229081622823099695104000000
Pieces placed: 26, Active States: 11378, Total W: 9359156643436224342346495426560000000
Pieces placed: 27, Active States:  8592, Total W: 131028193008107140792850935971840000000
Pieces placed: 28, Active States:  6446, Total W: 1703366509105392830307062167633920000000
Pieces placed: 29, Active States:  4712, Total W: 20440398109264713963684746011607040000000
Pieces placed: 30, Active States:  3429, Total W: 224844379201911853600532206127677440000000
Pieces placed: 31, Active States:  2412, Total W: 2248443792019118536005322061276774400000000
Pieces placed: 32, Active States:  1692, Total W: 20235994128172066824047898551490969600000000
Pieces placed: 33, Active States:  1130, Total W: 161887953025376534592383188411927756800000000
Pieces placed: 34, Active States:   759, Total W: 1133215671177635742146682318883494297600000000
Pieces placed: 35, Active States:   470, Total W: 6799294027065814452880093913300965785600000000
Pieces placed: 36, Active States:   297, Total W: 33996470135329072264400469566504828928000000000
Pieces placed: 37, Active States:   163, Total W: 135985880541316289057601878266019315712000000000
Pieces placed: 38, Active States:    94, Total W: 407957641623948867172805634798057947136000000000
Pieces placed: 39, Active States:    39, Total W: 815915283247897734345611269596115894272000000000
Pieces placed: 40, Active States:    20, Total W: 815915283247897734345611269596115894272000000000
Aggregating expectations...
Total valid permutations: 815915283247897734345611269596115894272000000000
Expected sum as Rational: 4471392366922179123435670680407386909235453/389058724998425357029729494855936000000000

User time    =       24.660
System time  =        0.382
Elapsed time =       24.874
Allocation   = 3336811896 bytes
107174 Page faults
GC time      =        0.913
 |------------------------------------------------------------|#
;;→ "11.492847"
:ok