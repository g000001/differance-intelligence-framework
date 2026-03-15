;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0098 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0098)

#||
(cl-text ARX-CORE-P98
  (cl-comment "Find max square from anagram word pairs.")
  (cl-comment "Word pair (W1, W2) must map to (S1, S2) such that both are squares.")
  (cl-comment "Substitution is a bijection: Char -> Digit.")
  (cl-comment "Leading zero is prohibited.")
)
||#


(defun get-word-pattern (word)
  "単語の文字パターンをリストで返す。例: CARE -> (0 1 2 3), ABBA -> (0 1 1 0)"
  (let ((mapping nil)
        (counter 0))
    (map 'list (lambda (c)
                 (let ((found (assoc c mapping)))
                   (if found
                       (cdr found)
                       (let ((val counter))
                         (push (cons c val) mapping)
                         (incf counter)
                         val))))
         word)))

(defun get-number-pattern (num-str)
  "数字文字列のパターンを返す。"
  (let ((mapping nil)
        (counter 0))
    (map 'list (lambda (c)
                 (let ((found (assoc c mapping)))
                   (if found
                       (cdr found)
                       (let ((val counter))
                         (push (cons c val) mapping)
                         (incf counter)
                         val))))
         num-str)))

(defun solve ()
  (let* ((words-string (read-file-into-string "0098_words.txt"))
         (words (mapcar (lambda (s) (string-trim "\"" s))
                        (cl-ppcre:split "," words-string)))
         (anagram-groups (make-hash-table :test 'equal))
         (max-square 0))

    ;; 1. アナグラムグループの抽出
    (iterate (for w in words)
             (let ((sorted-w (sort (copy-seq w) #'char<)))
               (push w (gethash sorted-w anagram-groups))))

    (let ((pairs nil))
      (iterate (for (key group) in-hashtable anagram-groups)
               (when (> (length group) 1)
                 ;; 組み合わせ(w1, w2)を生成。回文ではないアナグラムのみ。
                 (iterate (for sub on group)
                          (iterate (for w2 in (cdr sub))
                                   (push (cons (car sub) w2) pairs)))))

      ;; 2. 平方数のパターン別グループ化
      ;; 最大単語長を調べる
      (let* ((max-len (iterate (for (w1 . w2) in pairs) (maximize (length w1))))
             (squares-by-len (make-array (1+ max-len) :initial-element nil)))
        
        (format t "[Step 1] Precomputing squares up to length ~D...~%" max-len)
        (iterate (for i from 1)
                 (for sq = (* i i))
                 (for sq-str = (write-to-string sq))
                 (for len = (length sq-str))
                 (while (<= len max-len))
                 (push sq-str (aref squares-by-len len)))

        ;; 3. ペアの検証
        (format t "[Step 2] Testing anagram pairs...~%")
        (iterate (for (w1 . w2) in pairs)
                 (for len = (length w1))
                 (for p1 = (get-word-pattern w1))
                 (for sqs = (aref squares-by-len len))
                 
                 (format t "Checking pair: ~A - ~A (~D squares)~%" w1 w2 (length sqs))
                 
                 (iterate (for s1-str in sqs)
                          ;; パターンが一致するか
                          (when (equal p1 (get-number-pattern s1-str))
                            ;; 置換辞書を作成
                            (let ((char-to-digit (make-hash-table))
                                  (digit-to-char (make-hash-table)))
                              (iterate (for c in-vector w1)
                                       (for d in-vector s1-str)
                                       (setf (gethash c char-to-digit) d)
                                       (setf (gethash d digit-to-char) c))
                              
                              ;; w2 に適用
                              (let ((s2-str (map 'string (lambda (c) (gethash c char-to-digit)) w2)))
                                ;; s2 が0から始まらず、かつ平方数リストに含まれるか
                                (when (and (char/= (char s2-str 0) #\0)
                                           (member s2-str sqs :test #'string=))
                                  (let ((v1 (parse-integer s1-str))
                                        (v2 (parse-integer s2-str)))
                                    (setf max-square (max max-square v1 v2))))))))))
      
      (format t "[Complete]~%")
      max-square)))

;; 自己分析:
;; 1. 計算量削減の制約について:
;;    単語リスト(約2000語)からアナグラムペアを抽出すると、ペア数は非常に少なくなります。
;;    最大の長さの単語ペアはせいぜい数十の平方数候補しか持たないため、
;;    「文字パターン」によるフィルタリングを行うことで、各単語に対して検証すべき
;;    平方数は極めて限定的になります。
;; 2. 終了可能性:
;;    平方数の生成は O(sqrt(10^max_len)) であり、今回の max-len (約9) では
;;    10^4.5 程度のループであり瞬時に終わります。
;;    ペアのループと平方数のネストも、パターンマッチのおかげで事実上高速です。
;; 3. 罠:
;;    - 文字と数字の置換は「単射(one-to-one)」である必要があります。
;;      すなわち、異なる文字が同じ数字になってはいけません。
;;      本コードでは `get-number-pattern` と `get-word-pattern` を比較することで、
;;      文字の重複構造と数字の重複構造が完全に一致することを保証しています。
;;    - 先頭のゼロ(Leading zeroes)の禁止を忘れないこと。


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
[Step 1] Precomputing squares up to length 9...
[Step 2] Testing anagram pairs...
Checking pair: RACE - CARE (68 squares)
Checking pair: TONE - NOTE (68 squares)
Checking pair: GARDEN - DANGER (683 squares)
Checking pair: HEART - EARTH (217 squares)
Checking pair: FROM - FORM (68 squares)
Checking pair: GOD - DOG (22 squares)
Checking pair: EXPECT - EXCEPT (683 squares)
Checking pair: NEAR - EARN (68 squares)
Checking pair: TEA - EAT (22 squares)
Checking pair: OWN - NOW (22 squares)
Checking pair: WHO - HOW (22 squares)
Checking pair: LIFE - FILE (68 squares)
Checking pair: THESE - SHEET (217 squares)
Checking pair: THUS - SHUT (68 squares)
Checking pair: SOUTH - SHOUT (217 squares)
Checking pair: BROAD - BOARD (217 squares)
Checking pair: SEAT - EAST (68 squares)
Checking pair: THING - NIGHT (217 squares)
Checking pair: TIME - ITEM (68 squares)
Checking pair: CAT - ACT (22 squares)
Checking pair: STEAL - LEAST (217 squares)
Checking pair: SPOT - POST (68 squares)
Checking pair: STOP - POST (68 squares)
Checking pair: STOP - SPOT (68 squares)
Checking pair: QUITE - QUIET (217 squares)
Checking pair: ON - NO (6 squares)
Checking pair: USER - SURE (68 squares)
Checking pair: RECENT - CENTRE (683 squares)
Checking pair: SHAPE - PHASE (217 squares)
Checking pair: SING - SIGN (68 squares)
Checking pair: RAISE - ARISE (217 squares)
Checking pair: NAME - MEAN (68 squares)
Checking pair: LEAD - DEAL (68 squares)
Checking pair: HEAT - HATE (68 squares)
Checking pair: SOURCE - COURSE (683 squares)
Checking pair: REDUCTION - INTRODUCE (21623 squares)
Checking pair: REGION - IGNORE (683 squares)
Checking pair: WORTH - THROW (217 squares)
Checking pair: REFORM - FORMER (683 squares)
Checking pair: TEAR - RATE (68 squares)
Checking pair: MEAL - MALE (68 squares)
Checking pair: DIRECT - CREDIT (683 squares)
Checking pair: SIT - ITS (22 squares)
Checking pair: REACTION - CREATION (6837 squares)
[Complete]

User time    =        0.252
System time  =        0.022
Elapsed time =        1.157
Allocation   = 17522424 bytes
4284 Page faults
GC time      =        0.003
 |------------------------------------------------------------|#
;;→ 18769
:ok