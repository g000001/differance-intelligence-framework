;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0841 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0841)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p T)

(defconstant +pi-double+ 3.14159265358979323846d0)

(declaim (ftype (function (fixnum) (simple-array fixnum (*))) generate-fibs))
(defun generate-fibs (max-n)
  "フィボナッチ数列 F_1 から F_max-n までを生成する"
  (let ((fibs (make-array (1+ max-n) :element-type 'fixnum)))
    (setf (aref fibs 1) 1
          (aref fibs 2) 1)
    (iterate (for i from 3 to max-n)
             (setf (aref fibs i) (+ (aref fibs (- i 1))
                                    (aref fibs (- i 2)))))
    fibs))

(declaim (ftype (function (fixnum fixnum) double-float) calc-A))
(defun calc-A (p q)
  "公式 A(p, q) = p * [tan(q*pi/p) + 2 * sum_{j=1}^{q-1} (-1)^{q-j} tan(j*pi/p)] に基づき面積を計算。
   Kahan summation algorithm を用いて加算誤差を相殺する。"
  (declare (type fixnum p q))
  (let* ((pi/p (/ +pi-double+ (coerce p 'double-float)))
         (sum 0.0d0)
         (c 0.0d0)
         (sign -1.0d0))
    (declare (type double-float pi/p sum c sign))
    
    ;; 交代和の計算 (-1)^{q-j}
    ;; j を (q-1) から 1 へと降順に回すことで、初期符号(-1)から交互に反転させる
    (iterate (for j from (1- q) downto 1)
             (for term = (* sign (tan (* j pi/p))))
             (for y = (- term c))
             (for t-val = (+ sum y))
             (setf c (- (- t-val sum) y))
             (setf sum t-val)
             (setf sign (- sign)))
             
    (* p (+ (tan (* q pi/p)) (* 2.0d0 sum)))))

(defun solve ()
  "P841のメインルーチン。指定された範囲のフィボナッチ数列について総和を求める"
  (let ((fibs (generate-fibs 35))
        (total-sum 0.0d0)
        (c 0.0d0))
    (declare (type double-float total-sum c))
    
    ;; [自己批判・デバッグフェーズ] 問題文の既知の値を検算し、正当性原理を担保する
    (format t "--- 既知のテストケースによる数論的妥当性の検証 ---~%")
    (let ((test1 (calc-A 8 3))
          (test2 (calc-A 130021 50008)))
      (format t "Test A(8,3)           = ~,10F (Expected: 9.9411254970)~%" test1)
      (format t "Test A(130021,50008)  = ~,10F (Expected: 10.9210371479)~%" test2)
      (format t "--------------------------------------------------~%"))
    
    ;; 本計算フェーズ
    (iterate (for n from 3 to 34)
             (for p = (aref fibs (1+ n)))
             (for q = (aref fibs (1- n)))
             (for a-val = (calc-A p q))
             
             ;; 外周観測用のプリントデバッグ（10ステップごと、及び最終ステップに観測点を配置）
             (when (or (= (mod n 10) 0) (= n 34))
               (format t "[観測] n=~2D, p=~8D, q=~8D -> A = ~,10F~%" n p q a-val))
             
             ;; 総和時にもKahan summationを適用
             (for y = (- a-val c))
             (for t-val = (+ total-sum y))
             (setf c (- (- t-val total-sum) y))
             (setf total-sum t-val))
             
    (format t "--- 演算完了 ---~%")
    (format nil "~,10F" total-sum)))


#+| Do it | (solve )

;(format nil "~,10F" 381.78601328540367102103034853642215436995455664437d0)
;→ "381.7860132854"
;double    381.7860138865

#||
n= 3, p=       3, area=5.1961524227066318805823390245176171008284157614311
n= 4, p=       5, area=8.1229924058226581538967853053783616238725867880369
n= 5, p=       8, area=9.9411254969542811712405293810327538856721250090467
n= 6, p=      13, area=12.407241712337923429170866038137806939481848381897
n= 7, p=      21, area=9.4908066626897306591310920787118246724039279532852
n= 8, p=      34, area=13.28260996664082770747811606920389237953011293839
n= 9, p=      55, area=13.374330228595397714823549492345806866805273784304
n=10, p=      89, area=10.351472978034892121597258946920985688783862778288
n=11, p=     144, area=13.508869050122434325893415469351102409630311941583
n=12, p=     233, area=13.526800548802331303641759523455871413014975813746
n=13, p=     377, area=10.387727748394545012436787510671883358930200313814
n=14, p=     610, area=13.531916626752540417195726871068880275659521502784
n=15, p=     987, area=13.532280800323936199419951413128436108914776512174
n=16, p=    1597, area=10.391074616040680792983343734132524961732101055289
n=17, p=    2584, area=13.532718414956638975581407822622853642049569385779
n=18, p=    4181, area=13.532774512009953858292276615285060914769326615045
n=19, p=    6765, area=10.391189729800681706056483488528775839963011847705
n=20, p=   10946, area=13.53279045508342507195980280960871588121419983506
n=21, p=   17711, area=13.5327915870457149928832736586563403678655020086
n=22, p=   28657, area=10.391200133870674542566628226125652404690940054806
n=23, p=   46368, area=13.532792946319221628749746378058839817257934279757
n=24, p=   75025, area=13.532793120539354398345224349299028474925486641853
n=25, p=  121393, area=10.391200491395521462794553183679608413858053577469
n=26, p=  196418, area=13.532793170053011000061014315387630592970089381366
n=27, p=  317811, area=13.532793173568465181498123895183791402381587984348
n=28, p=  514229, area=10.391200523706705839008836785144793894343890986117
n=29, p=  832040, area=13.532793177789854203693445325605561905558196320783
n=30, p= 1346269, area=13.532793178330915773656334775167611322024699412131
n=31, p= 2178309, area=10.391200524817042403076498243913302781533624872894
n=32, p= 3524578, area=13.532793178484686381719151010597554025326993382593
n=33, p= 5702887, area=13.532793178495604046678359760918621045895680477632
n=34, p= 9227465, area=10.391200524917388664917667034580663958065729046369
--------------------------------------------------
Final Answer: 381.786013285403671021030348536422154369954556644
||#