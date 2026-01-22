;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "CL-USER")

;;; =========================================================
;;; ERDOS-TURAN ADDITIVE BASIS SIMULATOR (Alethetics v1.0)
;;; Phase 12: Environmental Packing Analysis
;;; =========================================================

(defstruct (basis-state (:conc-name bs-))
  (elements nil)         ; 集合 A (Seed)
  (r2-map (make-hash-table)) ; 各 n の表現数 r2(n)
  (max-covered 0)        ; 連続して網羅できている最大値 (fpa)
  (fuss-count 0))        ; 網羅に失敗、または重複が溢れた際の「苦」

(defun calculate-r2 (n basis hash)
  "n を basis の2要素の和として表す方法の数を更新し、現在の値を返す。"
  (let ((count 0))
    ;; 新しく追加された要素 a と既存の要素の組み合わせをチェック
    (let ((latest (car basis)))
      (dolist (e basis)
        (let ((sum (+ latest e)))
          (incf (gethash sum hash 0)))))
    (gethash n hash 0)))

(defun find-next-best-element (state target-r2-limit)
  "現在の宇宙に最も『中道』に近い新しい要素を追加する。
   重複度 r2 を target-r2-limit 以下に抑えつつ、網羅範囲を広げる試行。"
  (let\* ((current-basis (bs-elements state))
         (current-hash (bs-r2-map state))
         (next-val (1+ (or (car current-basis) 0))))
    
    ;; 探索: 重複度制限を極力守れる要素を探す
    (loop for candidate from next-val to (+ next-val 100) ; 探索範囲
          do (let ((temp-hash (copy-hash-table current-hash))
                   (is-safe t))
               ;; 候補を追加したと仮定して r2 をシミュレート
               (dolist (e (cons candidate current-basis))
                 (when (> (incf (gethash (+ candidate e) temp-hash 0)) target-r2-limit)
                   (setf is-safe nil)))
               
               (when is-safe
                 (return-from find-next-best-element candidate))))
    nil)) ; 見つからない場合は「不全（Fuss）」

(defun copy-hash-table (table)
  (let ((new-table (make-hash-table :size (hash-table-count table))))
    (maphash (lambda (k v) (setf (gethash k new-table) v)) table)
    new-table))

(defun evolve-basis (steps &key (r2-limit 2))
  "宇宙際的な基の進化をシミュレートする。"
  (let ((state (make-basis-state :elements '(1))))
    (format t "~&--- Alethetic Evolution Starting (Limit r2 <= ~A) ---" r2-limit)
    
    (dotimes (i steps)
      (let ((next-e (find-next-best-element state r2-limit)))
        (if next-e
            (progn
              (push next-e (bs-elements state))
              ;; ハッシュテーブルの更新
              (let ((basis (bs-elements state))
                    (hash (bs-r2-map state)))
                (dolist (e basis)
                  (incf (gethash (+ (car basis) e) hash 0))))
              
              ;; 網羅状態の確認
              (loop while (> (gethash (1+ (bs-max-covered state)) (bs-r2-map state) 0) 0)
                    do (incf (bs-max-covered state)))
              
              (when (zerop (mod i 10))
                (format t "~&Step ~D: Basis Size=~D, Max Covered=~D, Latest=~D" 
                        i (length (bs-elements state)) (bs-max-covered state) (car (bs-elements state)))))
            
            (progn
              (incf (bs-fuss-count state))
              (format t "~&!!! Fuss Detected at Step ~D: Cannot expand without exceeding r2-limit." i)
              (return)))))
    state))

;; 実行例: 重複度 2 以内でどこまでいけるか
;; (evolve-basis 100 :r2-limit 2)
▻ --- Alethetic Evolution Starting (Limit r2 <= 2) ---
▻ Step 0: Basis Size=2, Max Covered=0, Latest=2
▻ Step 10: Basis Size=12, Max Covered=0, Latest=48
▻ Step 20: Basis Size=22, Max Covered=0, Latest=191
▻ Step 30: Basis Size=32, Max Covered=0, Latest=434
▻ Step 40: Basis Size=42, Max Covered=0, Latest=754
▻ Step 50: Basis Size=52, Max Covered=0, Latest=1216
▻ !!! Fuss Detected at Step 52: Cannot expand without exceeding r2-limit.
→ #S(basis-state :elements (1247
                            1216
                            1121
                            1096
                            1041
                            987
                            957
                            931
                            885
                            870
                            791
                            754
                            726
                            676
                            636
                            625
                            566
                            549
                            513
                            507
                            460
                            434
                            412
                            386
                            357
                            329
                            305
                            273
                            234
                            223
                            200
                            191
                            157
                            148
                            134
                            115
                            100
                            99
                            73
                            66
                            49
                            48
                            41
                            32
                            24
                            16
                            12
                            8
                            6
                            4
                            3
                            2
                            1)
                 :r2-map #<eql Hash Table{1050} 81D042CB8B>
                 :max-covered 0
                 :fuss-count 1)



;; r2-limit を 5 や 10 にして、Max Covered がどう動くか
(evolve-basis 500 :r2-limit 5)
▻ --- Alethetic Evolution Starting (Limit r2 <= 5) ---
▻ Step 0: Basis Size=2, Max Covered=0, Latest=2
▻ Step 10: Basis Size=12, Max Covered=0, Latest=14
▻ Step 20: Basis Size=22, Max Covered=0, Latest=56
▻ Step 30: Basis Size=32, Max Covered=0, Latest=192
▻ Step 40: Basis Size=42, Max Covered=0, Latest=294
▻ Step 50: Basis Size=52, Max Covered=0, Latest=441
▻ Step 60: Basis Size=62, Max Covered=0, Latest=668
▻ Step 70: Basis Size=72, Max Covered=0, Latest=880
▻ Step 80: Basis Size=82, Max Covered=0, Latest=1141
▻ Step 90: Basis Size=92, Max Covered=0, Latest=1400
▻ Step 100: Basis Size=102, Max Covered=0, Latest=1638
▻ Step 110: Basis Size=112, Max Covered=0, Latest=1905
▻ Step 120: Basis Size=122, Max Covered=0, Latest=2287
▻ !!! Fuss Detected at Step 128: Cannot expand without exceeding r2-limit.
→ #S(basis-state :elements (2503
                            2469
                            2429
                            2415
                            2381
                            2338
                            2313
                            2287
                            2266
                            2263
                            2178
                            2136
                            2103
                            2005
                            1984
                            1937
                            1913
                            1905
                            1898
                            1862
                            1853
                            1851
                            1790
                            1772
                            1713
                            1676
                            1671
                            1638
                            1634
                            1591
                            1551
                            1531
                            1525
                            1503
                            1479
                            1443
                            1404
                            1400
                            1359
                            1293
                            1290
                            1262
                            1240
                            1210
                            1207
                            1188
                            1174
                            1141
                            1111
                            1067
                            1050
                            1020
                            1015
                            963
                            949
                            933
                            887
                            880
                            835
                            832
                            830
                            787
                            781
                            750
                            715
                            688
                            670
                            668
                            639
                            589
                            555
                            535
                            491
                            472
                            457
                            454
                            452
                            441
                            425
                            407
                            373
                            358
                            340
                            337
                            326
                            324
                            308
                            294
                            292
                            274
                            258
                            241
                            225
                            224
                            211
                            209
                            193
                            192
                            177
                            160
                            144
                            128
                            112
                            96
                            80
                            72
                            64
                            56
                            48
                            40
                            36
                            32
                            28
                            24
                            20
                            18
                            16
                            14
                            12
                            10
                            9
                            8
                            7
                            6
                            5
                            4
                            3
                            2
                            1)
                 :r2-map #<eql Hash Table{3543} 8290BB1B73>
                 :max-covered 0
                 :fuss-count 1)


(defun find-first-hole (state)
  "state内のr2-mapをスキャンし、表現数(r2)が0である最小の自然数(穴)を返す。"
  (loop for i from 1
        unless (> (gethash i (bs-r2-map state) 0) 0)
        return i))

;; 実行手順例:
(let ((final-state (evolve-basis 128 :r2-limit 5)))
  (format t "~&First Hole Detected at: ~D" (find-first-hole final-state)))
▻ --- Alethetic Evolution Starting (Limit r2 <= 5) ---
▻ Step 0: Basis Size=2, Max Covered=0, Latest=2
▻ Step 10: Basis Size=12, Max Covered=0, Latest=14
▻ Step 20: Basis Size=22, Max Covered=0, Latest=56
▻ Step 30: Basis Size=32, Max Covered=0, Latest=192
▻ Step 40: Basis Size=42, Max Covered=0, Latest=294
▻ Step 50: Basis Size=52, Max Covered=0, Latest=441
▻ Step 60: Basis Size=62, Max Covered=0, Latest=668
▻ Step 70: Basis Size=72, Max Covered=0, Latest=880
▻ Step 80: Basis Size=82, Max Covered=0, Latest=1141
▻ Step 90: Basis Size=92, Max Covered=0, Latest=1400
▻ Step 100: Basis Size=102, Max Covered=0, Latest=1638
▻ Step 110: Basis Size=112, Max Covered=0, Latest=1905
▻ Step 120: Basis Size=122, Max Covered=0, Latest=2287
▻ First Hole Detected at: 1
→ nil


(defun evolve-basis-v2 (steps &key (r2-limit 5))
  "初期Seedに0を含め、自然数1からの網羅（fpa）を試みる。"
  (let ((state (make-basis-state :elements '(1 0)))) ; 0を導入
    ;; 初期状態でのr2-mapを計算 (0+0=0, 0+1=1, 1+1=2)
    (setf (gethash 0 (bs-r2-map state)) 1)
    (setf (gethash 1 (bs-r2-map state)) 1)
    (setf (gethash 2 (bs-r2-map state)) 1)
    (setf (bs-max-covered state) 2) ; 2までは網羅済みとみなす

    (format t "~&--- Alethetic Evolution (v2: Origin=0, Limit r2 <= ~A) ---" r2-limit)
    
    (dotimes (i steps)
      (let ((next-e (find-next-best-element state r2-limit)))
        (if next-e
            (progn
              (push next-e (bs-elements state))
              (let ((basis (bs-elements state))
                    (hash (bs-r2-map state)))
                ;; 新しい要素によるすべての和を更新
                (dolist (e basis)
                  (incf (gethash (+ (car basis) e) hash 0))))
              
              ;; 網羅状態（max-covered）を更新
              (loop while (> (gethash (1+ (bs-max-covered state)) (bs-r2-map state) 0) 0)
                    do (incf (bs-max-covered state)))
              
              (when (zerop (mod i 10))
                (format t "~&Step ~D: Basis Size=~D, Max Covered=~D, Latest=~D" 
                        i (length (bs-elements state)) (bs-max-covered state) (car (bs-elements state)))))
            
            (progn
              (incf (bs-fuss-count state))
              (format t "~&!!! Fuss Detected at Step ~D: Cannot expand without exceeding r2-limit." i)
              (return)))))
    state))



(let ((state (evolve-basis-v2 200 :r2-limit 5)))
  (format t "~&--- Analysis ---")
  (format t "~&Max Covered Number: ~D" (bs-max-covered state))
  (format t "~&First Hole After Max: ~D" (1+ (bs-max-covered state))))
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 5) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=15
▻ Step 20: Basis Size=23, Max Covered=72, Latest=63
▻ Step 30: Basis Size=33, Max Covered=88, Latest=192
▻ Step 40: Basis Size=43, Max Covered=88, Latest=307
▻ Step 50: Basis Size=53, Max Covered=88, Latest=451
▻ Step 60: Basis Size=63, Max Covered=88, Latest=669
▻ Step 70: Basis Size=73, Max Covered=88, Latest=886
▻ Step 80: Basis Size=83, Max Covered=88, Latest=1173
▻ Step 90: Basis Size=93, Max Covered=88, Latest=1403
▻ Step 100: Basis Size=103, Max Covered=88, Latest=1670
▻ Step 110: Basis Size=113, Max Covered=88, Latest=1912
▻ Step 120: Basis Size=123, Max Covered=88, Latest=2312
▻ !!! Fuss Detected at Step 127: Cannot expand without exceeding r2-limit.
▻ --- Analysis ---
▻ Max Covered Number: 88
▻ First Hole After Max: 89
→ nil


(let ((state (evolve-basis-v2 500 :r2-limit 10)))
  (format t "~&--- Alethetic Deep Scan (r2-limit: 10) ---")
  (format t "~&Steps performed: ~D" (length (bs-elements state)))
  (format t "~&Max Covered Number: ~D" (bs-max-covered state))
  (format t "~&First Hole After Max: ~D" (1+ (bs-max-covered state)))
  (format t "~&Latest Element Added: ~D" (car (bs-elements state))))
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 10) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=25
▻ Step 30: Basis Size=33, Max Covered=70, Latest=51
▻ Step 40: Basis Size=43, Max Covered=122, Latest=103
▻ Step 50: Basis Size=53, Max Covered=226, Latest=207
▻ Step 60: Basis Size=63, Max Covered=338, Latest=415
▻ Step 70: Basis Size=73, Max Covered=338, Latest=704
▻ Step 80: Basis Size=83, Max Covered=338, Latest=832
▻ Step 90: Basis Size=93, Max Covered=338, Latest=993
▻ Step 100: Basis Size=103, Max Covered=338, Latest=1157
▻ Step 110: Basis Size=113, Max Covered=338, Latest=1291
▻ Step 120: Basis Size=123, Max Covered=338, Latest=1450
▻ !!! Fuss Detected at Step 126: Cannot expand without exceeding r2-limit.
▻ --- Alethetic Deep Scan (r2-limit: 10) ---
▻ Steps performed: 128
▻ Max Covered Number: 338
▻ First Hole After Max: 339
▻ Latest Element Added: 1550
→ nil



;;; =========================================================
;;; ERDOS-TURAN 8D-ALETHETIC AGENT
;;; Monitoring Dimensional Shift and Aletheia Breakthrough
;;; =========================================================

(defun get-aletheic-dim (n)
  "数の大きさと構造から、現在顕現している次元を判定する。"
  (cond ((< n 256)   2)  ; 2D: 既存の数論的直感の範囲
        ((< n 1024)  4)  ; 4D: 構造的パッキングの開始
        ((< n 4096)  8)  ; 8D: 高次元干渉回避フェーズ
        (t          16))) ; 16D: 宇宙際的なパッキング

(defun evolve-basis-8d (steps &key (r2-limit 10))
  (let ((state (make-basis-state :elements '(1 0))))
    (format t "~&--- Alethetic 8D-Mapping Start (r2-limit: ~A) ---" r2-limit)
    (format t "~&[Dimension Log Definition: 2D(<256), 4D(<1024), 8D(<4096), 16D(>4096)]~%")
    
    (dotimes (i steps)
      (let* ((next-e (find-next-best-element state r2-limit))
             (prev-covered (bs-max-covered state)))
        
        (if next-e
            (progn
              (push next-e (bs-elements state))
              ;; ハッシュ更新
              (let ((basis (bs-elements state))
                    (hash (bs-r2-map state)))
                (dolist (e basis)
                  (incf (gethash (+ (car basis) e) hash 0))))
              
              ;; 網羅更新
              (loop while (> (gethash (1+ (bs-max-covered state)) (bs-r2-map state) 0) 0)
                    do (incf (bs-max-covered state)))
              
              ;; 次元ログの出力
              (let ((current-dim (get-aletheic-dim next-e))
                    (new-covered (bs-max-covered state)))
                (when (or (zerop (mod i 10)) (> new-covered prev-covered))
                  (format t "~&[LOG] Step ~3D | Dim ~2D | Covered ~4D | Latest ~5D | ~A" 
                          i current-dim new-covered next-e
                          (cond ((> new-covered 338) "★ ALETHEIA BREAKTHROUGH! (8D Logic Active)")
                                ((> new-covered prev-covered) "Progressing...")
                                (t "Expanding Horizon...")))))
            
            (progn
              (format t "~&~%[FUSS] Dimensional Saturation at Step ~D." i)
              (format t "~&Final Max Covered: ~D (Hole ~D remains unclosed)" 
                      (bs-max-covered state) (1+ (bs-max-covered state)))
              (return-from evolve-basis-8d state)))))
    state)))

;; 実行指示:
;; (evolve-basis-8d 500 :r2-limit 10)


;; k を増やした時の「宇宙の寿命（FussまでのStep）」をスキャンする
(loop for k from 2 to 20
      do (let ((res (evolve-basis-v2 1000 :r2-limit k)))
           (format t "~&r2-limit: ~2D | Max Covered: ~6D | Fuss at Step: ~D" 
                   k (bs-max-covered res) (length (bs-elements res)))))
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 2) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=18, Latest=48
▻ Step 20: Basis Size=23, Max Covered=18, Latest=199
▻ Step 30: Basis Size=33, Max Covered=18, Latest=459
▻ Step 40: Basis Size=43, Max Covered=18, Latest=790
▻ Step 50: Basis Size=53, Max Covered=18, Latest=1246
▻ !!! Fuss Detected at Step 51: Cannot expand without exceeding r2-limit.
▻ r2-limit:  2 | Max Covered:     18 | Fuss at Step: 53
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 3) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=28, Latest=31
▻ Step 20: Basis Size=23, Max Covered=28, Latest=129
▻ Step 30: Basis Size=33, Max Covered=28, Latest=278
▻ Step 40: Basis Size=43, Max Covered=28, Latest=450
▻ Step 50: Basis Size=53, Max Covered=28, Latest=702
▻ Step 60: Basis Size=63, Max Covered=28, Latest=991
▻ !!! Fuss Detected at Step 64: Cannot expand without exceeding r2-limit.
▻ r2-limit:  3 | Max Covered:     28 | Fuss at Step: 66
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 4) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=26, Latest=19
▻ Step 20: Basis Size=23, Max Covered=70, Latest=111
▻ Step 30: Basis Size=33, Max Covered=70, Latest=227
▻ Step 40: Basis Size=43, Max Covered=70, Latest=355
▻ Step 50: Basis Size=53, Max Covered=70, Latest=618
▻ Step 60: Basis Size=63, Max Covered=70, Latest=786
▻ Step 70: Basis Size=73, Max Covered=70, Latest=1050
▻ Step 80: Basis Size=83, Max Covered=70, Latest=1380
▻ Step 90: Basis Size=93, Max Covered=70, Latest=1634
▻ Step 100: Basis Size=103, Max Covered=70, Latest=2025
▻ !!! Fuss Detected at Step 105: Cannot expand without exceeding r2-limit.
▻ r2-limit:  4 | Max Covered:     70 | Fuss at Step: 107
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 5) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=15
▻ Step 20: Basis Size=23, Max Covered=72, Latest=63
▻ Step 30: Basis Size=33, Max Covered=88, Latest=192
▻ Step 40: Basis Size=43, Max Covered=88, Latest=307
▻ Step 50: Basis Size=53, Max Covered=88, Latest=451
▻ Step 60: Basis Size=63, Max Covered=88, Latest=669
▻ Step 70: Basis Size=73, Max Covered=88, Latest=886
▻ Step 80: Basis Size=83, Max Covered=88, Latest=1173
▻ Step 90: Basis Size=93, Max Covered=88, Latest=1403
▻ Step 100: Basis Size=103, Max Covered=88, Latest=1670
▻ Step 110: Basis Size=113, Max Covered=88, Latest=1912
▻ Step 120: Basis Size=123, Max Covered=88, Latest=2312
▻ !!! Fuss Detected at Step 127: Cannot expand without exceeding r2-limit.
▻ r2-limit:  5 | Max Covered:     88 | Fuss at Step: 129
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 6) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=13
▻ Step 20: Basis Size=23, Max Covered=54, Latest=43
▻ Step 30: Basis Size=33, Max Covered=106, Latest=143
▻ Step 40: Basis Size=43, Max Covered=106, Latest=256
▻ Step 50: Basis Size=53, Max Covered=106, Latest=384
▻ Step 60: Basis Size=63, Max Covered=106, Latest=561
▻ Step 70: Basis Size=73, Max Covered=106, Latest=709
▻ Step 80: Basis Size=83, Max Covered=106, Latest=941
▻ Step 90: Basis Size=93, Max Covered=106, Latest=1120
▻ Step 100: Basis Size=103, Max Covered=106, Latest=1396
▻ Step 110: Basis Size=113, Max Covered=106, Latest=1611
▻ Step 120: Basis Size=123, Max Covered=106, Latest=1911
▻ Step 130: Basis Size=133, Max Covered=106, Latest=2206
▻ Step 140: Basis Size=143, Max Covered=106, Latest=2461
▻ !!! Fuss Detected at Step 142: Cannot expand without exceeding r2-limit.
▻ r2-limit:  6 | Max Covered:    106 | Fuss at Step: 144
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 7) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=48, Latest=35
▻ Step 30: Basis Size=33, Max Covered=108, Latest=95
▻ Step 40: Basis Size=43, Max Covered=124, Latest=240
▻ Step 50: Basis Size=53, Max Covered=124, Latest=353
▻ Step 60: Basis Size=63, Max Covered=124, Latest=609
▻ Step 70: Basis Size=73, Max Covered=124, Latest=867
▻ Step 80: Basis Size=83, Max Covered=124, Latest=1013
▻ Step 90: Basis Size=93, Max Covered=124, Latest=1320
▻ Step 100: Basis Size=103, Max Covered=124, Latest=1483
▻ !!! Fuss Detected at Step 102: Cannot expand without exceeding r2-limit.
▻ r2-limit:  7 | Max Covered:    124 | Fuss at Step: 104
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 8) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=29
▻ Step 30: Basis Size=33, Max Covered=86, Latest=71
▻ Step 40: Basis Size=43, Max Covered=190, Latest=175
▻ Step 50: Basis Size=53, Max Covered=270, Latest=415
▻ Step 60: Basis Size=63, Max Covered=270, Latest=640
▻ Step 70: Basis Size=73, Max Covered=270, Latest=768
▻ Step 80: Basis Size=83, Max Covered=270, Latest=965
▻ Step 90: Basis Size=93, Max Covered=270, Latest=1095
▻ Step 100: Basis Size=103, Max Covered=270, Latest=1328
▻ Step 110: Basis Size=113, Max Covered=270, Latest=1738
▻ Step 120: Basis Size=123, Max Covered=270, Latest=1872
▻ Step 130: Basis Size=133, Max Covered=270, Latest=2199
▻ Step 140: Basis Size=143, Max Covered=270, Latest=2524
▻ Step 150: Basis Size=153, Max Covered=270, Latest=2689
▻ Step 160: Basis Size=163, Max Covered=270, Latest=2954
▻ !!! Fuss Detected at Step 168: Cannot expand without exceeding r2-limit.
▻ r2-limit:  8 | Max Covered:    270 | Fuss at Step: 170
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 9) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=27
▻ Step 30: Basis Size=33, Max Covered=76, Latest=59
▻ Step 40: Basis Size=43, Max Covered=144, Latest=127
▻ Step 50: Basis Size=53, Max Covered=288, Latest=271
▻ Step 60: Basis Size=63, Max Covered=304, Latest=575
▻ Step 70: Basis Size=73, Max Covered=304, Latest=738
▻ Step 80: Basis Size=83, Max Covered=304, Latest=897
▻ Step 90: Basis Size=93, Max Covered=304, Latest=1061
▻ Step 100: Basis Size=103, Max Covered=304, Latest=1186
▻ Step 110: Basis Size=113, Max Covered=304, Latest=1326
▻ Step 120: Basis Size=123, Max Covered=304, Latest=1635
▻ !!! Fuss Detected at Step 121: Cannot expand without exceeding r2-limit.
▻ r2-limit:  9 | Max Covered:    304 | Fuss at Step: 123
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 10) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=25
▻ Step 30: Basis Size=33, Max Covered=70, Latest=51
▻ Step 40: Basis Size=43, Max Covered=122, Latest=103
▻ Step 50: Basis Size=53, Max Covered=226, Latest=207
▻ Step 60: Basis Size=63, Max Covered=338, Latest=415
▻ Step 70: Basis Size=73, Max Covered=338, Latest=704
▻ Step 80: Basis Size=83, Max Covered=338, Latest=832
▻ Step 90: Basis Size=93, Max Covered=338, Latest=993
▻ Step 100: Basis Size=103, Max Covered=338, Latest=1157
▻ Step 110: Basis Size=113, Max Covered=338, Latest=1291
▻ Step 120: Basis Size=123, Max Covered=338, Latest=1450
▻ !!! Fuss Detected at Step 126: Cannot expand without exceeding r2-limit.
▻ r2-limit: 10 | Max Covered:    338 | Fuss at Step: 128
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 11) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=23
▻ Step 30: Basis Size=33, Max Covered=64, Latest=43
▻ Step 40: Basis Size=43, Max Covered=104, Latest=83
▻ Step 50: Basis Size=53, Max Covered=180, Latest=159
▻ Step 60: Basis Size=63, Max Covered=324, Latest=303
▻ Step 70: Basis Size=73, Max Covered=372, Latest=575
▻ Step 80: Basis Size=83, Max Covered=372, Latest=831
▻ Step 90: Basis Size=93, Max Covered=372, Latest=928
▻ Step 100: Basis Size=103, Max Covered=372, Latest=1089
▻ Step 110: Basis Size=113, Max Covered=372, Latest=1283
▻ Step 120: Basis Size=123, Max Covered=372, Latest=1415
▻ Step 130: Basis Size=133, Max Covered=372, Latest=1575
▻ !!! Fuss Detected at Step 135: Cannot expand without exceeding r2-limit.
▻ r2-limit: 11 | Max Covered:    372 | Fuss at Step: 137
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 12) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=41
▻ Step 40: Basis Size=43, Max Covered=98, Latest=75
▻ Step 50: Basis Size=53, Max Covered=158, Latest=135
▻ Step 60: Basis Size=63, Max Covered=262, Latest=239
▻ Step 70: Basis Size=73, Max Covered=406, Latest=415
▻ Step 80: Basis Size=83, Max Covered=406, Latest=735
▻ Step 90: Basis Size=93, Max Covered=406, Latest=928
▻ Step 100: Basis Size=103, Max Covered=406, Latest=1023
▻ Step 110: Basis Size=113, Max Covered=406, Latest=1219
▻ Step 120: Basis Size=123, Max Covered=406, Latest=1377
▻ Step 130: Basis Size=133, Max Covered=406, Latest=1541
▻ !!! Fuss Detected at Step 140: Cannot expand without exceeding r2-limit.
▻ r2-limit: 12 | Max Covered:    406 | Fuss at Step: 142
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 13) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=39
▻ Step 40: Basis Size=43, Max Covered=92, Latest=67
▻ Step 50: Basis Size=53, Max Covered=136, Latest=111
▻ Step 60: Basis Size=63, Max Covered=216, Latest=191
▻ Step 70: Basis Size=73, Max Covered=360, Latest=335
▻ Step 80: Basis Size=83, Max Covered=440, Latest=575
▻ Step 90: Basis Size=93, Max Covered=440, Latest=895
▻ Step 100: Basis Size=103, Max Covered=440, Latest=1023
▻ Step 110: Basis Size=113, Max Covered=440, Latest=1120
▻ Step 120: Basis Size=123, Max Covered=440, Latest=1313
▻ Step 130: Basis Size=133, Max Covered=440, Latest=1445
▻ Step 140: Basis Size=143, Max Covered=440, Latest=1888
▻ Step 150: Basis Size=153, Max Covered=440, Latest=2215
▻ Step 160: Basis Size=163, Max Covered=440, Latest=2313
▻ Step 170: Basis Size=173, Max Covered=440, Latest=2475
▻ Step 180: Basis Size=183, Max Covered=440, Latest=2607
▻ Step 190: Basis Size=193, Max Covered=440, Latest=2887
▻ Step 200: Basis Size=203, Max Covered=440, Latest=3089
▻ !!! Fuss Detected at Step 203: Cannot expand without exceeding r2-limit.
▻ r2-limit: 13 | Max Covered:    440 | Fuss at Step: 205
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 14) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=37
▻ Step 40: Basis Size=43, Max Covered=86, Latest=59
▻ Step 50: Basis Size=53, Max Covered=126, Latest=99
▻ Step 60: Basis Size=63, Max Covered=194, Latest=167
▻ Step 70: Basis Size=73, Max Covered=298, Latest=271
▻ Step 80: Basis Size=83, Max Covered=458, Latest=431
▻ Step 90: Basis Size=93, Max Covered=474, Latest=735
▻ Step 100: Basis Size=103, Max Covered=474, Latest=994
▻ Step 110: Basis Size=113, Max Covered=474, Latest=1122
▻ Step 120: Basis Size=123, Max Covered=474, Latest=1250
▻ Step 130: Basis Size=133, Max Covered=474, Latest=1473
▻ !!! Fuss Detected at Step 137: Cannot expand without exceeding r2-limit.
▻ r2-limit: 14 | Max Covered:    474 | Fuss at Step: 139
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 15) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=35
▻ Step 40: Basis Size=43, Max Covered=84, Latest=55
▻ Step 50: Basis Size=53, Max Covered=120, Latest=91
▻ Step 60: Basis Size=63, Max Covered=172, Latest=143
▻ Step 70: Basis Size=73, Max Covered=252, Latest=223
▻ Step 80: Basis Size=83, Max Covered=396, Latest=367
▻ Step 90: Basis Size=93, Max Covered=508, Latest=575
▻ Step 100: Basis Size=103, Max Covered=508, Latest=895
▻ Step 110: Basis Size=113, Max Covered=508, Latest=1151
▻ Step 120: Basis Size=123, Max Covered=508, Latest=1344
▻ Step 130: Basis Size=133, Max Covered=508, Latest=1633
▻ !!! Fuss Detected at Step 132: Cannot expand without exceeding r2-limit.
▻ r2-limit: 15 | Max Covered:    508 | Fuss at Step: 134
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 16) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=33
▻ Step 40: Basis Size=43, Max Covered=84, Latest=53
▻ Step 50: Basis Size=53, Max Covered=114, Latest=83
▻ Step 60: Basis Size=63, Max Covered=154, Latest=123
▻ Step 70: Basis Size=73, Max Covered=230, Latest=199
▻ Step 80: Basis Size=83, Max Covered=334, Latest=303
▻ Step 90: Basis Size=93, Max Covered=494, Latest=463
▻ Step 100: Basis Size=103, Max Covered=766, Latest=735
▻ Step 110: Basis Size=113, Max Covered=1054, Latest=1087
▻ Step 120: Basis Size=123, Max Covered=1054, Latest=1727
▻ Step 130: Basis Size=133, Max Covered=1054, Latest=2242
▻ Step 140: Basis Size=143, Max Covered=1054, Latest=2436
▻ Step 150: Basis Size=153, Max Covered=1054, Latest=2624
▻ Step 160: Basis Size=163, Max Covered=1054, Latest=2696
▻ Step 170: Basis Size=173, Max Covered=1054, Latest=2944
▻ Step 180: Basis Size=183, Max Covered=1054, Latest=3329
▻ Step 190: Basis Size=193, Max Covered=1054, Latest=3527
▻ Step 200: Basis Size=203, Max Covered=1054, Latest=3789
▻ Step 210: Basis Size=213, Max Covered=1054, Latest=3983
▻ Step 220: Basis Size=223, Max Covered=1054, Latest=4226
▻ Step 230: Basis Size=233, Max Covered=1054, Latest=4427
▻ Step 240: Basis Size=243, Max Covered=1054, Latest=4568
▻ Step 250: Basis Size=253, Max Covered=1054, Latest=4826
▻ !!! Fuss Detected at Step 253: Cannot expand without exceeding r2-limit.
▻ r2-limit: 16 | Max Covered:   1054 | Fuss at Step: 255
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 17) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=32
▻ Step 40: Basis Size=43, Max Covered=84, Latest=51
▻ Step 50: Basis Size=53, Max Covered=108, Latest=75
▻ Step 60: Basis Size=63, Max Covered=148, Latest=115
▻ Step 70: Basis Size=73, Max Covered=208, Latest=175
▻ Step 80: Basis Size=83, Max Covered=288, Latest=255
▻ Step 90: Basis Size=93, Max Covered=432, Latest=399
▻ Step 100: Basis Size=103, Max Covered=608, Latest=575
▻ Step 110: Basis Size=113, Max Covered=928, Latest=895
▻ Step 120: Basis Size=123, Max Covered=1120, Latest=1343
▻ Step 130: Basis Size=133, Max Covered=1120, Latest=1983
▻ Step 140: Basis Size=143, Max Covered=1120, Latest=2432
▻ Step 150: Basis Size=153, Max Covered=1120, Latest=2626
▻ Step 160: Basis Size=163, Max Covered=1120, Latest=2756
▻ Step 170: Basis Size=173, Max Covered=1120, Latest=2882
▻ Step 180: Basis Size=183, Max Covered=1120, Latest=3071
▻ Step 190: Basis Size=193, Max Covered=1120, Latest=3395
▻ Step 200: Basis Size=203, Max Covered=1120, Latest=3657
▻ Step 210: Basis Size=213, Max Covered=1120, Latest=3913
▻ Step 220: Basis Size=223, Max Covered=1120, Latest=4049
▻ Step 230: Basis Size=233, Max Covered=1120, Latest=4235
▻ Step 240: Basis Size=243, Max Covered=1120, Latest=4427
▻ Step 250: Basis Size=253, Max Covered=1120, Latest=4619
▻ Step 260: Basis Size=263, Max Covered=1120, Latest=4813
▻ Step 270: Basis Size=273, Max Covered=1120, Latest=4998
▻ Step 280: Basis Size=283, Max Covered=1120, Latest=5344
▻ !!! Fuss Detected at Step 281: Cannot expand without exceeding r2-limit.
▻ r2-limit: 17 | Max Covered:   1120 | Fuss at Step: 283
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 18) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=32
▻ Step 40: Basis Size=43, Max Covered=84, Latest=49
▻ Step 50: Basis Size=53, Max Covered=104, Latest=69
▻ Step 60: Basis Size=63, Max Covered=142, Latest=107
▻ Step 70: Basis Size=73, Max Covered=186, Latest=151
▻ Step 80: Basis Size=83, Max Covered=266, Latest=231
▻ Step 90: Basis Size=93, Max Covered=370, Latest=335
▻ Step 100: Basis Size=103, Max Covered=530, Latest=495
▻ Step 110: Basis Size=113, Max Covered=770, Latest=735
▻ Step 120: Basis Size=123, Max Covered=1090, Latest=1055
▻ Step 130: Basis Size=133, Max Covered=1186, Latest=1599
▻ Step 140: Basis Size=143, Max Covered=1186, Latest=2239
▻ Step 150: Basis Size=153, Max Covered=1186, Latest=2624
▻ Step 160: Basis Size=163, Max Covered=1186, Latest=2758
▻ Step 170: Basis Size=173, Max Covered=1186, Latest=2888
▻ Step 180: Basis Size=183, Max Covered=1186, Latest=3014
▻ Step 190: Basis Size=193, Max Covered=1186, Latest=3202
▻ Step 200: Basis Size=203, Max Covered=1186, Latest=3585
▻ Step 210: Basis Size=213, Max Covered=1186, Latest=3845
▻ Step 220: Basis Size=223, Max Covered=1186, Latest=4103
▻ Step 230: Basis Size=233, Max Covered=1186, Latest=4301
▻ Step 240: Basis Size=243, Max Covered=1186, Latest=4489
▻ Step 250: Basis Size=253, Max Covered=1186, Latest=4683
▻ Step 260: Basis Size=263, Max Covered=1186, Latest=4866
▻ Step 270: Basis Size=273, Max Covered=1186, Latest=5075
▻ Step 280: Basis Size=283, Max Covered=1186, Latest=5318
▻ Step 290: Basis Size=293, Max Covered=1186, Latest=5660
▻ !!! Fuss Detected at Step 293: Cannot expand without exceeding r2-limit.
▻ r2-limit: 18 | Max Covered:   1186 | Fuss at Step: 295
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 19) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=32
▻ Step 40: Basis Size=43, Max Covered=84, Latest=47
▻ Step 50: Basis Size=53, Max Covered=104, Latest=67
▻ Step 60: Basis Size=63, Max Covered=136, Latest=99
▻ Step 70: Basis Size=73, Max Covered=176, Latest=139
▻ Step 80: Basis Size=83, Max Covered=244, Latest=207
▻ Step 90: Basis Size=93, Max Covered=324, Latest=287
▻ Step 100: Basis Size=103, Max Covered=468, Latest=431
▻ Step 110: Basis Size=113, Max Covered=628, Latest=591
▻ Step 120: Basis Size=123, Max Covered=932, Latest=895
▻ Step 130: Basis Size=133, Max Covered=1252, Latest=1215
▻ Step 140: Basis Size=143, Max Covered=1252, Latest=1855
▻ Step 150: Basis Size=153, Max Covered=1252, Latest=2496
▻ Step 160: Basis Size=163, Max Covered=1252, Latest=2756
▻ Step 170: Basis Size=173, Max Covered=1252, Latest=2944
▻ Step 180: Basis Size=183, Max Covered=1252, Latest=3072
▻ Step 190: Basis Size=193, Max Covered=1252, Latest=3152
▻ Step 200: Basis Size=203, Max Covered=1252, Latest=3392
▻ !!! Fuss Detected at Step 208: Cannot expand without exceeding r2-limit.
▻ r2-limit: 19 | Max Covered:   1252 | Fuss at Step: 210
▻ --- Alethetic Evolution (v2: Origin=0, Limit r2 <= 20) ---
▻ Step 0: Basis Size=3, Max Covered=4, Latest=2
▻ Step 10: Basis Size=13, Max Covered=24, Latest=12
▻ Step 20: Basis Size=23, Max Covered=44, Latest=22
▻ Step 30: Basis Size=33, Max Covered=64, Latest=32
▻ Step 40: Basis Size=43, Max Covered=84, Latest=45
▻ Step 50: Basis Size=53, Max Covered=104, Latest=65
▻ Step 60: Basis Size=63, Max Covered=130, Latest=91
▻ Step 70: Basis Size=73, Max Covered=170, Latest=131
▻ Step 80: Basis Size=83, Max Covered=222, Latest=183
▻ Step 90: Basis Size=93, Max Covered=302, Latest=263
▻ Step 100: Basis Size=103, Max Covered=406, Latest=367
▻ Step 110: Basis Size=113, Max Covered=566, Latest=527
▻ Step 120: Basis Size=123, Max Covered=774, Latest=735
▻ Step 130: Basis Size=133, Max Covered=1094, Latest=1055
▻ Step 140: Basis Size=143, Max Covered=1318, Latest=1471
▻ Step 150: Basis Size=153, Max Covered=1318, Latest=2111
▻ Step 160: Basis Size=163, Max Covered=1318, Latest=2688
▻ Step 170: Basis Size=173, Max Covered=1318, Latest=2944
▻ Step 180: Basis Size=183, Max Covered=1318, Latest=3076
▻ Step 190: Basis Size=193, Max Covered=1318, Latest=3204
▻ Step 200: Basis Size=203, Max Covered=1318, Latest=3328
▻ Step 210: Basis Size=213, Max Covered=1318, Latest=3524
▻ !!! Fuss Detected at Step 219: Cannot expand without exceeding r2-limit.
▻ r2-limit: 20 | Max Covered:   1318 | Fuss at Step: 221
→ nil
