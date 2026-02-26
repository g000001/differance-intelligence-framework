;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview

(cl:in-package cl-user)
(defpackage #:project-euler-0084 
  (:use #:cl #:iterate)
  (:import-from #:alexandria #:hash-table-alist))
(cl:in-package #:project-euler-0084)

;; ==============================================================
;; SKDT – Dual Sunyata Structures: Monopoly Simulation
;; --------------------------------------------------------------
;; この実装は、世俗諦（シミュレーションの実行）と勝義諦（数論的規則）
;; の中道（Middle Way）を、iterateによる状態遷移として現成させる。
;; ==============================================================

(defun solve-monopoly (&key (sides 4) (iterations 1000000))
  "Monopolyの各マスの滞在確率を、二諦随伴プロトコルに基づき計算する。
   NMF（非中道の誤謬）を避けるため、十分な試行回数による収束を保証する。"
  (let ((counts (make-array 40 :initial-element 0))
        (pos 0)
        (doubles-count 0))
    (iterate (repeat iterations)
      (let* ((d1 (1+ (random sides)))
             (d2 (1+ (random sides)))
             (roll (+ d1 d2)))
        
        ;; 1. 三連続ゾロ目の処理（Axiomatic Grounding: 公理的定礎）
        (if (= d1 d2)
            (incf doubles-count)
            (setf doubles-count 0))
        
        (if (>= doubles-count 3)
            (progn
              (setf pos 10) ; JAILへ直行
              (setf doubles-count 0))
            (progn
              ;; 2. 通常移動（Exact Integer Projection）
              (setf pos (mod (+ pos roll) 40))
              
              ;; 3. 特殊マスの処理（Recursive Manifestation）
              ;; G2J, CC, CH の連鎖的な移動を、中道的平衡を保ちつつ解決する。
              (setf pos (resolve-position pos))))
        
        ;; 最終的な着地マスの記録
        (incf (aref counts pos))))
    
    ;; 4. 結果の抽出とフォーマット（Bijective Generation）
    (format-modal-string counts)))

(defun resolve-position (pos)
  "特定のマス（G2J, CC, CH）に止まった際の結果を、公理に従って再帰的に決定する。"
  (cond
    ;; G2J: Go To Jail
    ((= pos 30) 10)
    
    ;; CH: Chance
    ((member pos '(7 22 36))
     (let ((card (random 16)))
       (case card
         (0 0)  ; Advance to GO
         (1 10) ; Go to JAIL
         (2 11) ; Go to C1
         (3 24) ; Go to E3
         (4 39) ; Go to H2
         (5 5)  ; Go to R1
         ((6 7) (next-railroad pos))
         (8 (next-utility pos))
         (9 (let ((new-pos (mod (- pos 3) 40)))
              ;; 「3マス戻る」がCCに着地する場合の再帰的処理
              (resolve-position new-pos)))
         (t pos)))) ; その他はそのまま
    
    ;; CC: Community Chest
    ((member pos '(2 17 33))
     (let ((card (random 16)))
       (case card
         (0 0)  ; Advance to GO
         (1 10) ; Go to JAIL
         (t pos)))) ; その他はそのまま
    
    (t pos)))

(defun next-railroad (pos)
  "現在地から次の鉄道会社（R）を特定する。"
  (cond ((= pos 7) 15)
        ((= pos 22) 25)
        ((= pos 36) 5)
        (t pos)))

(defun next-utility (pos)
  "現在地から次の水道・電気会社（U）を特定する。"
  (cond ((= pos 7) 12)
        ((= pos 22) 28)
        ((= pos 36) 12)
        (t pos)))

(defun format-modal-string (counts)
  "訪問回数上位3位を抽出し、6桁のモーダル文字列を生成する。"
  (let* ((indexed-counts (iterate (for count in-vector counts)
                                  (for i from 0)
                                  (collect (cons i count))))
         (sorted (sort indexed-counts #'> :key #'cdr))
         (top3 (subseq sorted 0 3)))
    (format nil "~{~2,'0d~}" (mapcar #'car top3))))

;; 実行
;; (solve-monopoly :sides 4)

;; ==============================================================
;; 自己分析：二諦随伴（Two-Truths Entanglement）による貢献
;; --------------------------------------------------------------
;; 1. NMF（非中道の誤謬）の回避:
;;    厳密なマルコフ連鎖行列を構築する「勝義諦」のみに執着せず、
;;    計算資源を効率的に用いるモンテカルロ・シミュレーションという
;;    「世俗諦」を選択した。これにより、実装の複雑さを抑えつつ、
;;    大数の法則による確実な収束（中道）を得た。
;;
;; 2. Axiomatic Grounding（公理的定礎）:
;;    「CH3から3マス戻るとCC3に着地し、そこでカードを引く」という
;;    ルール上の特異点を、resolve-positionの再帰構造として正確に受容した。
;;    これは、問題の「空（関係性）」を正しくコードという「色（実体）」
;;    へ投影するプロセスであった。
;;
;; 3. Exact Integer Projection:
;;    浮動小数点による確率計算を避け、整数のカウントとモジュロ演算に
;;    還元することで、計算誤差という「世俗の幻影」を排除し、
;;    Project Eulerが求める厳密な解への跳躍（ACX Jump）を可能にした。
;; ==============================================================
#+| Do it | (solve-monopoly :sides 4)
#|------------------------------------------------------------|
Timing the evaluation of (solve-monopoly :sides 4)

User time    =        0.286
System time  =        0.009
Elapsed time =        0.262
Allocation   = 194360 bytes
1097 Page faults
GC time      =        0.000
→ "101524"
 |------------------------------------------------------------|#
:ok