;;; -*- mode: Lisp; coding: utf-8  -*-

(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)
                   (debug 0)))

(cl:in-package "CL-USER")


;;;; ======================================================================
;;;; 16マトリックス・トランスレータ: Prolog to Common Lisp
;;;; ターゲット: ゼブラ問題 (Einstein's Puzzle)
;;;; 戦略: 14. 絶の非 (CPS) + 10. 亦の非 (Destructive Backtracking)
;;;; ======================================================================

(defun unify (pattern target)
  "2. 是の非(変数)を 1. 是の是(事実)で埋める破壊的単一化。
   不整合(Fuss)が生じた場合は即座にnilを返し、成功時はtを返す。"
  (loop for p in pattern
        for t-ptr on target
        do (let ((t-val (car t-ptr)))
             (cond
               ((and p t-val (not (eq p t-val))) (return-from unify nil))
               ((and p (null t-val)) (setf (car t-ptr) p))))
        finally (return t)))

(defun zebra (cont)
  "14. 絶の非 (CPS): 継続の連鎖によって推論空間を構成する。"
  (let ((h (loop repeat 5 collect (list nil nil nil nil nil))))
    ;; 位相1: 初期事実の導入 (Norwegian / Milk)
    (setf (first (first h)) 'norwegian)
    (setf (fourth (third h)) 'milk)

    (labels 
        ((member-csp (x list next-cont)
           (declare (type list list)
                    (type function next-cont))
           (dolist (house list)
             (let ((snapshot (copy-list house))) ; 10. 亦の非: 状態の保存
               (when (unify x house)
                 (funcall next-cont))
               (replace house snapshot)))) ; 10. 亦の非: 状態の復元(Debt精算)

         (iright-csp (l r list next-cont)
           (loop for (a b) on list while b do
             (let ((snap-a (copy-list a)) (snap-b (copy-list b)))
               (when (and (unify l a) (unify r b))
                 (funcall next-cont))
               (replace a snap-a) (replace b snap-b))))

         (nextto-csp (x y list next-cont)
           (iright-csp x y list next-cont)
           (iright-csp y x list next-cont)))

      ;; --- 推論ラインの顕現 (Prologの述語順序を忠実にCPS化) ---
      (member-csp '(englishman nil nil nil red) h (lambda ()
      (member-csp '(spaniard dog nil nil nil) h (lambda ()
      (member-csp '(nil nil nil coffee green) h (lambda ()
      (member-csp '(ukrainian nil nil tea nil) h (lambda ()
      (iright-csp '(nil nil nil nil ivory) '(nil nil nil nil green) h (lambda ()
      (member-csp '(nil snails winston nil nil) h (lambda ()
      (member-csp '(nil nil kools nil yellow) h (lambda ()
      (nextto-csp '(nil nil chesterfield nil nil) '(nil fox nil nil nil) h (lambda ()
      (nextto-csp '(nil nil kools nil nil) '(nil horse nil nil nil) h (lambda ()
      (member-csp '(nil nil luckystrike oj nil) h (lambda ()
      (member-csp '(japanese nil parliaments nil nil) h (lambda ()
      (nextto-csp '(norwegian nil nil nil nil) '(nil nil nil nil blue) h (lambda ()
      (member-csp '(nil nil nil water nil) h (lambda () ;; WaterDrinkerの探索
      (member-csp '(nil zebra nil nil nil) h (lambda () ;; ZebraOwnerの探索
        ;; 16. 絶の絶: すべての制約を満たした最終状態を継続へ渡す
        (funcall cont h))))))))))))))))))))))))))))))))

(defun zebra-solve ()
  "8. 非の絶 (Cut): 探索空間を打ち切り、不動点を抽出する。"
  (block found
    (zebra (lambda (h)
             (let ((w-owner (first (find 'water h :key (lambda (x) (fourth x)))))
                   (z-owner (first (find 'zebra h :key (lambda (x) (second x))))))
               ;; 不動点の現成と外部への射影
               (return-from found (values (copy-tree h) w-owner z-owner)))))
    nil))

(defun benchmark-zebra (&optional (iterations 1000))
  "Prolog版 benchmark/0 との比較用実行関数。"
  (format t "Starting Zebra Benchmark (~A iterations)...~%" iterations)
  (time 
   (dotimes (i iterations)
     (zebra-solve)))
  (multiple-value-bind (houses water zebra) (zebra-solve)
    (format t "~%[Result]~%")
    (format t "Water Drinker: ~A~%" water)
    (format t "Zebra Owner  : ~A~%" zebra)
    (format t "House Details:~%")
    (dolist (house houses) (format t "  ~A~%" house))))

#||
;; 実行コマンド:
(time (benchmark-zebra 1000))
▻ Starting Zebra Benchmark (1000 iterations)...
▻ 
▻ [Result]
▻ Water Drinker: norwegian
▻ Zebra Owner  : japanese
▻ House Details:
▻   (norwegian fox kools water yellow)
▻   (ukrainian horse chesterfield tea blue)
▻   (englishman snails winston milk red)
▻   (spaniard dog luckystrike oj ivory)
▻   (japanese zebra parliaments coffee green)
→ nil
User time    =        0.898
System time  =        0.004
Elapsed time =        0.892
Allocation   = 1109182440 bytes
304 Page faults
GC time      =        0.008


||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; International Allegro CL Free Express Edition 11.0 [64-bit macOS (Intel)]
;;      ZEBRA-ALLEGRO-PROLOG:    11,985,981LIPS owner: JAPANESE 8.23PP
;;                 ZEBRA-CPS:     9,226,618LIPS owner: JAPANESE 6.34PP
;;                   ZEBRA-4:     6,107,142LIPS owner: JAPANESE 4.20PP
;;    ZEBRA-ALLEGRO-MODERATO:     5,457,446LIPS owner: JAPANESE 3.75PP
;;           ZEBRA-VPROLOG-T:     3,271,683LIPS owner: JAPANESE 2.25PP
;;                   ZEBRA-5:     3,246,835LIPS owner: JAPANESE 2.23PP
;;                   ZEBRA-1:     2,806,345LIPS owner: JAPANESE 1.93PP
;;             ZEBRA-VPROLOG:     2,734,541LIPS owner: JAPANESE 1.88PP
;;                   ZEBRA-3:     2,261,904LIPS owner: JAPANESE 1.55PP
;;                   ZEBRA-2:     2,055,288LIPS owner: JAPANESE 1.41PP
;;                   ZEBRA-0:     1,781,250LIPS owner: JAPANESE 1.22PP
;;                   ZEBRA-6:     1,617,276LIPS owner: JAPANESE 1.11PP
;;           ZEBRA-PAIPROLOG:     1,455,732LIPS owner: JAPANESE 1.00PP
;;            ZEBRA-ZRPROLOG:     1,417,127LIPS owner: JAPANESE 0.97PP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LispWorks 8.1.2
;;                 ZEBRA-CPS:    14,140,022LIPS owner: japanese 9.23PP
;;           ZEBRA-VPROLOG-T:     5,247,545LIPS owner: japanese 3.43PP
;;                ZEBRA-CLOG:     5,015,643LIPS owner: japanese 3.27PP
;;                   ZEBRA-4:     4,075,309LIPS owner: japanese 2.66PP
;;             ZEBRA-VPROLOG:     2,941,513LIPS owner: japanese 1.92PP
;;                   ZEBRA-3:     2,364,055LIPS owner: japanese 1.54PP
;;                   ZEBRA-1:     2,064,220LIPS owner: japanese 1.35PP
;;                   ZEBRA-5:     2,038,302LIPS owner: japanese 1.33PP
;;                   ZEBRA-0:     1,785,714LIPS owner: japanese 1.17PP
;;                   ZEBRA-2:     1,726,807LIPS owner: japanese 1.13PP
;;           ZEBRA-PAIPROLOG:     1,532,075LIPS owner: japanese 1.00PP
;;            ZEBRA-ZRPROLOG:     1,500,175LIPS owner: japanese 0.98PP
;;                   ZEBRA-6:     1,455,566LIPS owner: japanese 0.95PP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SBCL 2.5.4
;;                 ZEBRA-CPS:    15,851,944LIPS owner: JAPANESE 4.03PP
;;           ZEBRA-VPROLOG-T:     9,527,976LIPS owner: JAPANESE 2.43PP
;;                   ZEBRA-4:     8,410,922LIPS owner: JAPANESE 2.14PP
;;                   ZEBRA-3:     7,859,703LIPS owner: JAPANESE 2.00PP
;;             ZEBRA-VPROLOG:     7,454,644LIPS owner: JAPANESE 1.90PP
;;                   ZEBRA-2:     7,138,205LIPS owner: JAPANESE 1.82PP
;;                   ZEBRA-0:     6,298,404LIPS owner: JAPANESE 1.60PP
;;                   ZEBRA-6:     5,534,895LIPS owner: JAPANESE 1.41PP
;;                   ZEBRA-5:     4,553,085LIPS owner: JAPANESE 1.16PP
;;            ZEBRA-ZRPROLOG:     4,047,777LIPS owner: JAPANESE 1.03PP
;;           ZEBRA-PAIPROLOG:     3,928,865LIPS owner: JAPANESE 1.00PP
;;                   ZEBRA-1:     3,490,652LIPS owner: JAPANESE 0.89PP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clozure Common Lisp Version 1.12.2  DarwinX8664
;;                 ZEBRA-CPS:     6,337,201LIPS owner: JAPANESE 4.29PP
;;           ZEBRA-VPROLOG-T:     3,195,993LIPS owner: JAPANESE 2.17PP
;;             ZEBRA-VPROLOG:     2,485,747LIPS owner: JAPANESE 1.68PP
;;           ZEBRA-PAIPROLOG:     1,476,135LIPS owner: JAPANESE 1.00PP
;;            ZEBRA-ZRPROLOG:     1,337,609LIPS owner: JAPANESE 0.91PP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECL 23.9.9
;;                   ZEBRA-4:     4,084,100LIPS owner: JAPANESE 21.96PP
;;                   ZEBRA-1:     1,865,646LIPS owner: JAPANESE 10.03PP
;;                 ZEBRA-CPS:     1,196,431LIPS owner: JAPANESE 6.43PP
;;                   ZEBRA-5:       740,996LIPS owner: JAPANESE 3.98PP
;;                   ZEBRA-3:       703,350LIPS owner: JAPANESE 3.78PP
;;                   ZEBRA-2:       614,230LIPS owner: JAPANESE 3.30PP
;;             ZEBRA-VPROLOG:       519,973LIPS owner: JAPANESE 2.80PP
;;           ZEBRA-VPROLOG-T:       518,374LIPS owner: JAPANESE 2.79PP
;;                   ZEBRA-6:       266,447LIPS owner: JAPANESE 1.43PP
;;                   ZEBRA-0:       194,786LIPS owner: JAPANESE 1.05PP
;;           ZEBRA-PAIPROLOG:       186,021LIPS owner: JAPANESE 1.00PP
;;            ZEBRA-ZRPROLOG:       177,800LIPS owner: JAPANESE 0.96PP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; swi-prolog
;; % 12,877,724 inferences, 1.487 CPU in 1.504 seconds (99% CPU, 8658073 Lips)
