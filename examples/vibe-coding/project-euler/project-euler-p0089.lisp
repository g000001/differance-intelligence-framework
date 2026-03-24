;;; -*- mode: Lisp; coding: utf-8 -*-
;;; llm-model: claude-sonnet-4-6
(cl:in-package cl-user)
(defpackage #:project-euler-0089 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0089)

#||
(cl:comment "PE 089 CLIF Logic Definition"

;; ローマ数字の値テーブル
(forall (c)
  (iff (roman-value c v)
       (or (and (= c "I") (= v 1))
           (and (= c "V") (= v 5))
           (and (= c "X") (= v 10))
           (and (= c "L") (= v 50))
           (and (= c "C") (= v 100))
           (and (= c "D") (= v 500))
           (and (= c "M") (= v 1000)))))

;; ローマ数字文字列 → 整数値
;; 減算記法：左の文字が右より小さければ引く
(forall (s n)
  (iff (roman-to-int s n)
       (= n (sum-with-subtractive-rule s))))

;; 整数値 → 最小ローマ数字文字列
;; 貪欲法：大きい単位から順に割り当てる
(forall (n s)
  (iff (int-to-minimal-roman n s)
       (greedy-assign n
         ((1000 "M") (900 "CM") (500 "D") (400 "CD")
          (100 "C")  (90  "XC") (50  "L") (40  "XL")
          (10  "X")  (9   "IX") (5   "V") (4   "IV") (1 "I")))))

;; 節約文字数
(forall (line saved)
  (iff (chars-saved line saved)
       (let ((n (roman-to-int line)))
         (= saved (- (length line)
                     (length (int-to-minimal-roman n)))))))

;; 答え
(= answer (sum (line in file) (chars-saved line))))
||#


(defparameter *roman-values*
  '((#\M . 1000) (#\D . 500) (#\C . 100)
    (#\L . 50)   (#\X . 10)  (#\V . 5) (#\I . 1)))

(defparameter *minimal-roman-table*
  ;; 貪欲法テーブル：値の降順
  '((1000 . "M")  (900 . "CM") (500 . "D")  (400 . "CD")
    (100  . "C")  (90  . "XC") (50  . "L")  (40  . "XL")
    (10   . "X")  (9   . "IX") (5   . "V")  (4   . "IV")
    (1    . "I")))

(defun roman-char-value (c)
  (cdr (assoc c *roman-values*)))

(defun roman-to-integer (roman-string)
  "減算記法を正しく処理してローマ数字文字列を整数に変換する"
  (let ((chars (coerce roman-string 'list))
        (total 0))
    (iterate
      (while chars)
      (let* ((current-val (roman-char-value (first chars)))
             (next-val    (if (second chars)
                              (roman-char-value (second chars))
                              0)))
        (if (< current-val next-val)
            ;; 減算記法: IV=4, IX=9, XL=40 等
            (progn
              (incf total (- next-val current-val))
              (setf chars (cddr chars)))
            ;; 通常加算
            (progn
              (incf total current-val)
              (setf chars (cdr chars))))))
    total))

(defun integer-to-minimal-roman (n)
  "整数を最小ローマ数字文字列に変換する（貪欲法）"
  (let ((result (make-array 20 :element-type 'character
                               :fill-pointer 0
                               :adjustable t)))
    (iterate (for (value . numerals) in *minimal-roman-table*)
      (iterate (while (>= n value))
        (decf n value)
        (iterate (for c in-vector numerals)
          (vector-push-extend c result))))
    result))

(defun 0089_roman.txt ()
  (read-file-into-string "0089_roman.txt"))

(defun solve ()
  (let ((input-path "0089_roman.txt")
        (total-saved 0)
        (line-count 0))
    (format t "Reading ~A ...~%" input-path)
    
    (with-input-from-string (stream (0089_roman.txt))
      (iterate
        (for line = (read-line stream nil nil))
        (while line)
        ;; 末尾の空白・改行を除去
        (let* ((trimmed   (string-trim '(#\Space #\Return #\Newline) line))
               (original-length (length trimmed)))
          (when (> original-length 0)
            (let* ((value         (roman-to-integer trimmed))
                   (minimal       (integer-to-minimal-roman value))
                   (minimal-length (length minimal))
                   (saved         (- original-length minimal-length)))
              (incf total-saved saved)
              (incf line-count)
              (when (> saved 0)
                (format t "  ~A -> ~A (~A) saved=~A~%"
                        trimmed minimal value saved)))))))

    (format t "Lines processed: ~A~%" line-count)
    (format t "Total characters saved: ~A~%" total-saved)
    total-saved))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Reading 0089_roman.txt ...
  MMMDLXVIIII -> MMMDLXIX (3569) saved=3
  MMCCCLXXXXIX -> MMCCCXCIX (2399) saved=3
  MDCCCXXIIII -> MDCCCXXIV (1824) saved=2
  MMMMDCCCCI -> MMMMCMI (4901) saved=3
  MCCLXXVIIII -> MCCLXXIX (1279) saved=3
  MMMMCCXXXXI -> MMMMCCXLI (4241) saved=2
  MMMDCCCCXXXIV -> MMMCMXXXIV (3934) saved=3
  CDXVIIII -> CDXIX (419) saved=3
  MMMMCCCLXXXXVI -> MMMMCCCXCVI (4396) saved=3
  MMMDCCCVIIII -> MMMDCCCIX (3809) saved=3
  DCCLXXXIIII -> DCCLXXXIV (784) saved=2
  MDCCCCXXXII -> MCMXXXII (1932) saved=3
  MMMMCMLXXXXVIII -> MMMMCMXCVIII (4998) saved=3
  MMDCCCLXXXIIII -> MMDCCCLXXXIV (2884) saved=2
  MMCCCCXXXXV -> MMCDXLV (2445) saved=4
  MMMMDLXXXVIIII -> MMMMDLXXXIX (4589) saved=3
  MMDCCCCLXXVI -> MMCMLXXVI (2976) saved=3
  MCCCCLXX -> MCDLXX (1470) saved=2
  MMCDLVIIII -> MMCDLIX (2459) saved=3
  MMMMCCCCXIX -> MMMMCDXIX (4419) saved=2
  MMMMCDLXXXIIII -> MMMMCDLXXXIV (4484) saved=2
  MMMCCCCXXX -> MMMCDXXX (3430) saved=2
  CLXXXXI -> CXCI (191) saved=3
  DLXXXX -> DXC (590) saved=3
  MMMMDLXXXXVIII -> MMMMDXCVIII (4598) saved=3
  DCCCCXLIII -> CMXLIII (943) saved=3
  MMMMCCCCXI -> MMMMCDXI (4411) saved=2
  MMCCCCXI -> MMCDXI (2411) saved=2
  MMMMDXXXXII -> MMMMDXLII (4542) saved=2
  MMMCCCCXV -> MMMCDXV (3415) saved=2
  MMDCCCCXVI -> MMCMXVI (2916) saved=3
  CCLIIII -> CCLIV (254) saved=2
  MMDCCLXXXVIIII -> MMDCCLXXXIX (2789) saved=3
  MMCCLXIIII -> MMCCLXIV (2264) saved=2
  CMLXXXXI -> CMXCI (991) saved=3
  MXVIIII -> MXIX (1019) saved=3
  MCCCCLXVIII -> MCDLXVIII (1468) saved=2
  MMMDCCLXXIIII -> MMMDCCLXXIV (3774) saved=2
  MMMMCCCLXXXXVII -> MMMMCCCXCVII (4397) saved=3
  MMDCCLXXXXV -> MMDCCXCV (2795) saved=3
  MMMCCCCXXVIIII -> MMMCDXXIX (3429) saved=5
  MMDXXXXI -> MMDXLI (2541) saved=2
  MDCCCCXV -> MCMXV (1915) saved=3
  MMMMCMLXXXXV -> MMMMCMXCV (4995) saved=3
  DCCCCIIII -> CMIV (904) saved=5
  MMCCCCIII -> MMCDIII (2403) saved=2
  MMMDCCLXXXVIIII -> MMMDCCLXXXIX (3789) saved=3
  MDCCCLXXXXV -> MDCCCXCV (1895) saved=3
  MMDLXXXIIII -> MMDLXXXIV (2584) saved=2
  MMMDCCCXLIIII -> MMMDCCCXLIV (3844) saved=2
  DCCCCLXVII -> CMLXVII (967) saved=3
  MMMCLXXXXIII -> MMMCXCIII (3193) saved=3
  MMMMDCLXXXXV -> MMMMDCXCV (4695) saved=3
  MMMMDCXXVIIII -> MMMMDCXXIX (4629) saved=3
  DCCCCXXXVIIII -> CMXXXIX (939) saved=6
  MMCCXXXIIII -> MMCCXXXIV (2234) saved=2
  MDCCLXVIIII -> MDCCLXIX (1769) saved=3
  DXXXXIII -> DXLIII (543) saved=2
  MMMMDCCIIII -> MMMMDCCIV (4704) saved=2
  MMMCCVIIII -> MMMCCIX (3209) saved=3
  MCCCCLXXIX -> MCDLXXIX (1479) saved=2
  MMMMDCCCCXXXVI -> MMMMCMXXXVI (4936) saved=3
  MMMDCCCXXXXV -> MMMDCCCXLV (3845) saved=2
  MMDCXXXXIV -> MMDCXLIV (2644) saved=2
  MDCCCLXXXX -> MDCCCXC (1890) saved=3
  MMDCXXXXVIIII -> MMDCXLIX (2649) saved=5
  MDXCVIIII -> MDXCIX (1599) saved=3
  MDCLVIIII -> MDCLIX (1659) saved=3
  MMMCCCLXIIII -> MMMCCCLXIV (3364) saved=2
  MMIIII -> MMIV (2004) saved=2
  MMMMCCCCXXXII -> MMMMCDXXXII (4432) saved=2
  MXVIIII -> MXIX (1019) saved=3
  MMMCCCCXIIII -> MMMCDXIV (3414) saved=4
  CCCCLXIV -> CDLXIV (464) saved=2
  MMXXXXII -> MMXLII (2042) saved=2
  MMMMCCLXXXX -> MMMMCCXC (4290) saved=3
  CCCCLVIIII -> CDLIX (459) saved=5
  MCDLXXXXIV -> MCDXCIV (1494) saved=3
  MMCMLXIIII -> MMCMLXIV (2964) saved=2
  MMMCCCCLXXI -> MMMCDLXXI (3471) saved=2
  MMDCCCCXVIII -> MMCMXVIII (2918) saved=3
  MMCCXLVIIII -> MMCCXLIX (2249) saved=3
  MMMDCCCLIIII -> MMMDCCCLIV (3854) saved=2
  MMDCXXXXI -> MMDCXLI (2641) saved=2
  MMMCCCCXXXII -> MMMCDXXXII (3432) saved=2
  DCCCCLXXXXII -> CMXCII (992) saved=6
  MLXXXXVII -> MXCVII (1097) saved=3
  MMMLXVIIII -> MMMLXIX (3069) saved=3
  MDCCCCXXX -> MCMXXX (1930) saved=3
  DCCCLXXVIIII -> DCCCLXXIX (879) saved=3
  MMDXXXXIX -> MMDXLIX (2549) saved=2
  MMCLXXXXV -> MMCXCV (2195) saved=3
  MDCCXXXXIII -> MDCCXLIII (1743) saved=2
  MMMCLXXXXIIII -> MMMCXCIV (3194) saved=5
  MDCCCCXXXIII -> MCMXXXIII (1933) saved=3
  MCCCXXXXIIII -> MCCCXLIV (1344) saved=4
  MMDCCCCIV -> MMCMIV (2904) saved=3
  MMMDCCLXVIIII -> MMMDCCLXIX (3769) saved=3
  DLXXXXIX -> DXCIX (599) saved=3
  MXXXIIII -> MXXXIV (1034) saved=2
  MMMDCCLXXXIIII -> MMMDCCLXXXIV (3784) saved=2
  DCCCXXXX -> DCCCXL (840) saved=2
  MMCDXXXVIIII -> MMCDXXXIX (2439) saved=3
  MMMDCCCCVI -> MMMCMVI (3906) saved=3
  MMCMLXVIIII -> MMCMLXIX (2969) saved=3
  MMCCCCLXI -> MMCDLXI (2461) saved=2
  MMMCCCXXXIIII -> MMMCCCXXXIV (3334) saved=2
  MMDCCLXXXXI -> MMDCCXCI (2791) saved=3
  MMDCCCXXXXVI -> MMDCCCXLVI (2846) saved=2
  CDXIIII -> CDXIV (414) saved=2
  MLXIIII -> MLXIV (1064) saved=2
  CCCCLXVI -> CDLXVI (466) saved=2
  MMMMCCCCXXXVI -> MMMMCDXXXVI (4436) saved=2
  CCCCLXXXX -> CDXC (490) saved=5
  MCCCLXXXXII -> MCCCXCII (1392) saved=3
  DCCXLIIII -> DCCXLIV (744) saved=2
  MMMMDCCCCLXIII -> MMMMCMLXIII (4963) saved=3
  MMMDCLIIII -> MMMDCLIV (3654) saved=2
  MCCCCXXXXII -> MCDXLII (1442) saved=4
  CCCCLIII -> CDLIII (453) saved=2
  MMDCCCCLX -> MMCMLX (2960) saved=3
  MMMCCCLXXXX -> MMMCCCXC (3390) saved=3
  MDCCCCXXVI -> MCMXXVI (1926) saved=3
  MMDCCCCLXXVI -> MMCMLXXVI (2976) saved=3
  MMMDCCLXXVIIII -> MMMDCCLXXIX (3779) saved=3
  MMMCCCCLXVII -> MMMCDLXVII (3467) saved=2
  LXXVIIII -> LXXIX (79) saved=3
  MMDCCCXXXXVIII -> MMDCCCXLVIII (2848) saved=2
  MMDCCLXXXXVII -> MMDCCXCVII (2797) saved=3
  DCLXXXXI -> DCXCI (691) saved=3
  MMCCCLXXXXVIII -> MMCCCXCVIII (2398) saved=3
  MDCCCCLXXVIII -> MCMLXXVIII (1978) saved=3
  MMXVIIII -> MMXIX (2019) saved=3
  MMCCCCXXVI -> MMCDXXVI (2426) saved=2
  MXXXXII -> MXLII (1042) saved=2
  MMDCCCCLXXXII -> MMCMLXXXII (2982) saved=3
  MMMDCCCCLII -> MMMCMLII (3952) saved=3
  MMMCCLXVIIII -> MMMCCLXIX (3269) saved=3
  MMMCCCCLXVI -> MMMCDLXVI (3466) saved=2
  MMMMDCLXXVIIII -> MMMMDCLXXIX (4679) saved=3
  MMMMDCXXXXIV -> MMMMDCXLIV (4644) saved=2
  MDXVIIII -> MDXIX (1519) saved=3
  MMMCLXXXXV -> MMMCXCV (3195) saved=3
  CCCCXX -> CDXX (420) saved=2
  MDCCCLXXIIII -> MDCCCLXXIV (1874) saved=2
  MMMDCCCCLXXXVII -> MMMCMLXXXVII (3987) saved=3
  MMMDCCLXXXXVI -> MMMDCCXCVI (3796) saved=3
  MMMMCCCCLIIII -> MMMMCDLIV (4454) saved=4
  MCMLXXXXIII -> MCMXCIII (1993) saved=3
  MDCCCXXXXIV -> MDCCCXLIV (1844) saved=2
  XXXXVII -> XLVII (47) saved=2
  MDCXXVIIII -> MDCXXIX (1629) saved=3
  MMMCXXXXIIII -> MMMCXLIV (3144) saved=4
  MMMMCCCCLXI -> MMMMCDLXI (4461) saved=2
  DCLXXXXVIIII -> DCXCIX (699) saved=6
  LXXXXI -> XCI (91) saved=3
  MDCXXXXII -> MDCXLII (1642) saved=2
  MMMMLXXXX -> MMMMXC (4090) saved=3
  MMDCCCCXLV -> MMCMXLV (2945) saved=3
  MMDCCCCLX -> MMCMLX (2960) saved=3
  DCCCCXXIII -> CMXXIII (923) saved=3
  MMMMDCCXLVIIII -> MMMMDCCXLIX (4749) saved=3
  MMMMDCCCLXVIIII -> MMMMDCCCLXIX (4869) saved=3
  MCCCCII -> MCDII (1402) saved=2
  MMDCLIIII -> MMDCLIV (2654) saved=2
  MLXXIIII -> MLXXIV (1074) saved=2
  MMCCCCVI -> MMCDVI (2406) saved=2
  MMMMDCCCCXCV -> MMMMCMXCV (4995) saved=3
  MMMMDCCCCXXXX -> MMMMCMXL (4940) saved=5
  MMCCCCLX -> MMCDLX (2460) saved=2
  MDCCCCII -> MCMII (1902) saved=3
  MMCLXXXXVIII -> MMCXCVIII (2198) saved=3
  MDCCCCLV -> MCMLV (1955) saved=3
  MCCCCLXXIIII -> MCDLXXIV (1474) saved=4
  MDCCCCXLIII -> MCMXLIII (1943) saved=3
  CLXXXXV -> CXCV (195) saved=3
  MCCCCLXIX -> MCDLXIX (1469) saved=2
  MCCCLXXXXIIII -> MCCCXCIV (1394) saved=5
  MMDCCCXLVIIII -> MMDCCCXLIX (2849) saved=3
  MMMMCCCXXXXVIII -> MMMMCCCXLVIII (4348) saved=2
  MMMMDCLXXIIII -> MMMMDCLXXIV (4674) saved=2
  MMCCCXXIIII -> MMCCCXXIV (2324) saved=2
  CCCCXXXXV -> CDXLV (445) saved=4
  MMMCCCCLXV -> MMMCDLXV (3465) saved=2
  MMMMDCCCXXXXII -> MMMMDCCCXLII (4842) saved=2
  MMDCCCCLXXXXVI -> MMCMXCVI (2996) saved=6
  MMMMDCCCCLXXXV -> MMMMCMLXXXV (4985) saved=3
  MMCCXXXXVIII -> MMCCXLVIII (2248) saved=2
  MMMMLXXXXVIIII -> MMMMXCIX (4099) saved=6
  MMMCCCCLXXII -> MMMCDLXXII (3472) saved=2
  MMCXXXXVIIII -> MMCXLIX (2149) saved=5
  MMMMDCXXXIIII -> MMMMDCXXXIV (4634) saved=2
  MCDLXXXXIII -> MCDXCIII (1493) saved=3
  MCCCCXCIII -> MCDXCIII (1493) saved=2
  DCCCXXXXIIII -> DCCCXLIV (844) saved=4
  MCCCCXXIIII -> MCDXXIV (1424) saved=4
  MMMMCXXXXIIII -> MMMMCXLIV (4144) saved=4
  MCCCCXXIV -> MCDXXIV (1424) saved=2
  MCCCLIIII -> MCCCLIV (1354) saved=2
  MMMCCIIII -> MMMCCIV (3204) saved=2
  DCCLXXXVIIII -> DCCLXXXIX (789) saved=3
  MDVIIII -> MDIX (1509) saved=3
  MMMCDLVIIII -> MMMCDLIX (3459) saved=3
  MMCCCCVII -> MMCDVII (2407) saved=2
  MXXXXII -> MXLII (1042) saved=2
  MMMMDCXXXXI -> MMMMDCXLI (4641) saved=2
  MMMMDCCCXXXXV -> MMMMDCCCXLV (4845) saved=2
  MMMMCCXVIIII -> MMMMCCXIX (4219) saved=3
  MMDCCXIIII -> MMDCCXIV (2714) saved=2
  MDCCLVIIII -> MDCCLIX (1759) saved=3
  MMCXXIIII -> MMCXXIV (2124) saved=2
  MMMCLIIII -> MMMCLIV (3154) saved=2
  MMMMCLXXXX -> MMMMCXC (4190) saved=3
  MMMCLXXXIIII -> MMMCLXXXIV (3184) saved=2
  MMMDXXXXIII -> MMMDXLIII (3543) saved=2
  MMMMCCCCLV -> MMMMCDLV (4455) saved=2
  MCCCCLXXX -> MCDLXXX (1480) saved=2
  MMDCCCCXXXI -> MMCMXXXI (2931) saved=3
  MMMDCLXXXVIIII -> MMMDCLXXXIX (3689) saved=3
  MMMDLXXXXVII -> MMMDXCVII (3597) saved=3
  MDLXIIII -> MDLXIV (1564) saved=2
  MDCCCXXXXVI -> MDCCCXLVI (1846) saved=2
  MMMMDCIIII -> MMMMDCIV (4604) saved=2
  XXXXVI -> XLVI (46) saved=2
  MCMLVIIII -> MCMLIX (1959) saved=3
  CLXXXXIX -> CXCIX (199) saved=3
  MMMDCCCCLVIII -> MMMCMLVIII (3958) saved=3
  MCCCC -> MCD (1400) saved=2
  MMMCCLXVIIII -> MMMCCLXIX (3269) saved=3
  MMMDCCXXXXI -> MMMDCCXLI (3741) saved=2
  MMMMXXXXVI -> MMMMXLVI (4046) saved=2
  MCCCCLXXXVIIII -> MCDLXXXIX (1489) saved=5
  CCCLXXXXIX -> CCCXCIX (399) saved=3
  CCCCXIII -> CDXIII (413) saved=2
  CCCCXVI -> CDXVI (416) saved=2
  MDCCCLXXXIIII -> MDCCCLXXXIV (1884) saved=2
  MMMMCCCCLXXXI -> MMMMCDLXXXI (4481) saved=2
  MMCCCLXXVIIII -> MMCCCLXXIX (2379) saved=3
  MMMDCCCLVIIII -> MMMDCCCLIX (3859) saved=3
  MMMMCXXXXV -> MMMMCXLV (4145) saved=2
  CCCCLIX -> CDLIX (459) saved=2
  MMMCCCXXXXV -> MMMCCCXLV (3345) saved=2
  MDCCCCXCV -> MCMXCV (1995) saved=3
  MMDCCCXCIIII -> MMDCCCXCIV (2894) saved=2
  MMMCDXXIIII -> MMMCDXXIV (3424) saved=2
  MMMMDCCCCLXVIII -> MMMMCMLXVIII (4968) saved=3
  MMMXXIIII -> MMMXXIV (3024) saved=2
  DCCCXXXXVIII -> DCCCXLVIII (848) saved=2
  MMMDCCCCXXXVIIII -> MMMCMXXXIX (3939) saved=6
  MCLXXXXIIII -> MCXCIV (1194) saved=5
  DXXXXIII -> DXLIII (543) saved=2
  MCCCXXXXVIII -> MCCCXLVIII (1348) saved=2
  MCLXXXXVIII -> MCXCVIII (1198) saved=3
  CCLXVIIII -> CCLXIX (269) saved=3
  MCCCCLXXXI -> MCDLXXXI (1481) saved=2
  MMMCCCXXVIIII -> MMMCCCXXIX (3329) saved=3
  MMMCCXXVIIII -> MMMCCXXIX (3229) saved=3
  MMMCCCCXXXXVI -> MMMCDXLVI (3446) saved=4
  CDLXXXXIII -> CDXCIII (493) saved=3
  DCCXXXIIII -> DCCXXXIV (734) saved=2
  MDCCXVIIII -> MDCCXIX (1719) saved=3
  MDCCCCXXXVII -> MCMXXXVII (1937) saved=3
  MMDXCVIIII -> MMDXCIX (2599) saved=3
  MMMDCCCCVIII -> MMMCMVIII (3908) saved=3
  MMMMDCCCCXXXXVI -> MMMMCMXLVI (4946) saved=5
  MCMLXXXXIII -> MCMXCIII (1993) saved=3
  XXXXVIIII -> XLIX (49) saved=5
Lines processed: 1000
Total characters saved: 743

User time    =        0.072
System time  =        0.017
Elapsed time =        1.052
Allocation   = 4295822096 bytes
3785 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 743
:ok

#||
## 自己分析

### 問題文に含まれていた計算量削減のための制約

1. **ファイルサイズが1000行・11KB**
   → 入力サイズが極めて小さい。計算量の問題は存在しない。
   → 1行あたりの処理はO(長さ)で、ローマ数字の最大長は~15文字程度。
   → 全体でO(1000 * 15) = O(15000)、ミリ秒以下で完結する。

2. **「valid but not necessarily minimal」という保証**
   → 入力が有効なローマ数字であることが保証されている。
   → バリデーション不要。roman-to-integerが正しく動けば十分。

3. **「no more than four consecutive identical units」という注記**
   → 入力の複雑さの上限を示している。
   → 特殊ケースの処理が不要であることの保証。

### 生成したコードが現実的な時間で終了しない可能性

**終了しない可能性：ほぼゼロ**

- ファイルが存在しない場合はエラーで停止（無限ループにはならない）
- integer-to-minimal-romanのwhileループは必ずnが減少するので停止する
- roman-to-integerのwhileループは文字列長だけ反復するので停止する

計算量：O(1000)、実測で1秒以内確実。

### 本問題にはLLMが陥りやすい罠はあるか

**罠あり：2点**

1. **減算記法の処理ミス**
   IVを「I=1, V=5」と単純加算すると6になる。
   左の文字が右より小さいときに引くという処理を見落としやすい。
   本コードでは next-val との比較で正しく処理している。

2. **最小化テーブルの不完全さ**
   900(CM), 400(CD), 90(XC), 40(XL), 9(IX), 4(IV) の
   減算記法エントリを忘れると最小化が不完全になる。
   LLMは 1000,500,100,50,10,5,1 の7エントリだけを
   列挙して残り6エントリを忘れがち。
   本コードでは13エントリ全てを網羅している。
||#
