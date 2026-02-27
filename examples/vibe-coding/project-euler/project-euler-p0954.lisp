;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0954 (:use cl #:iterate))
(in-package #:project-euler-0954)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; -------------------------------------------------------------
;;; 基礎関数
;;; -------------------------------------------------------------
(declaim (inline get-w get-v))

(defun get-w (id)
  "クラスIDに対する組み合わせ重み (1,8 と 2,9 は2通り)"
  (declare (type fixnum id))
  (if (or (= id 1) (= id 2)) 2 1))

(defun get-v (id)
  "クラスIDに対する mod 7 での値 (ID=7 は非ゼロの7の倍数)"
  (declare (type fixnum id))
  (if (= id 7) 0 id))

(defparameter *p3* (make-array 6 :element-type 'fixnum :initial-contents '(1 3 2 6 4 5)))
(defparameter *inv* (make-array 7 :element-type 'fixnum :initial-contents '(0 1 4 5 2 3 6)))

;;; -------------------------------------------------------------
;;; 禁止マスクの事前計算
;;; -------------------------------------------------------------
(defparameter *precomp*
  (let ((arr (make-array (list 7 6 8) :element-type '(unsigned-byte 64) :initial-element 0)))
    (iterate (for R from 1 to 6)
      (iterate (for mA from 0 to 5)
        (iterate (for idA from 0 to 7)
          (let ((vA (get-v idA))
                (forb-mask 0))
            (iterate (for mB from (1+ mA) to 5) ; 未来の桁への制約のみ記録
              (let* ((diff (mod (- (aref *p3* mB) (aref *p3* mA)) 7))
                     (inv-diff (aref *inv* diff))
                     (v-target (mod (+ vA (* R inv-diff)) 7))
                     (mask 0))
                (iterate (for idB from 0 to 7)
                  (when (= (get-v idB) v-target)
                    (setf mask (logior mask (ash 1 idB)))))
                (setf forb-mask (logior forb-mask (ash mask (* mB 8))))))
            (setf (aref arr R mA idA) forb-mask)))))
    arr))

;;; -------------------------------------------------------------
;;; メイン求解
;;; -------------------------------------------------------------
(defun solve-p954 (&optional (max-len 13))
  (let ((ans 0)
        (p3 *p3*)
        (precomp *precomp*)
        (inv *inv*))
    (declare (type (simple-array fixnum (6)) p3)
             (type (simple-array (unsigned-byte 64) (7 6 8)) precomp)
             (type (simple-array fixnum (7)) inv)
             (type integer ans))
    
    (iterate (for L from 1 to max-len)
      (let ((M (mod (1- L) 6))
            (K (make-array 6 :element-type 'fixnum :initial-element 0)))
        
        ;; 各 m に配置する桁数を算出
        (iterate (for p from 0 to (- L 2))
          (incf (aref K (mod p 6))))
        
        ;; 先頭の桁 D (1..9)
        (iterate (for D from 1 to 9)
          (let ((vD (mod D 7)))
            
            ;; 目標となる全体の余り R を固定 (1..6)
            (iterate (for R from 1 to 6)
              
              ;; 先頭桁 D が後続の桁に与える禁止マスクを生成
              (let ((init-forb 0))
                (iterate (for mB from 0 to 5)
                  (unless (= mB M)
                    (let* ((diff (mod (- (aref p3 mB) (aref p3 M)) 7))
                           (inv-diff (aref inv diff))
                           (v-target (mod (+ vD (* R inv-diff)) 7))
                           (mask 0))
                      ;; 0とのスワップは無効(条件外)なので idB=1 から
                      (iterate (for idB from 1 to 7)
                        (when (= (get-v idB) v-target)
                          (setf mask (logior mask (ash 1 idB)))))
                      (setf init-forb (logior init-forb (ash mask (* mB 8)))))))
                
                ;; 超高速DFS
                (labels ((dfs (m sum forb)
                           (declare (type fixnum m sum)
                                    (type (unsigned-byte 64) forb))
                           (if (= m 6)
                               (if (= sum R) 1 0)
                               (let ((k-count (aref K m))
                                     (res 0)
                                     ;; 現在の桁 m で許可されているクラスを取得
                                     (allowed (logandc2 255 (logand 255 (ash forb (* -8 m))))))
                                 (declare (type fixnum k-count allowed)
                                          (type integer res))
                                 (cond
                                  ((= k-count 0)
                                   (setf res (dfs (1+ m) sum forb)))
                                   
                                  ((= k-count 1)
                                   (iterate (for id1 from 0 to 7)
                                     (when (plusp (logand allowed (ash 1 id1)))
                                       (let ((nsum (mod (+ sum (* (get-v id1) (aref p3 m))) 7))
                                             (nforb (logior forb (aref precomp R m id1))))
                                         (incf res (* (get-w id1) (dfs (1+ m) nsum nforb)))))))
                                   
                                  ((= k-count 2)
                                   (iterate (for id1 from 0 to 7)
                                     (when (plusp (logand allowed (ash 1 id1)))
                                       (iterate (for id2 from id1 to 7)
                                         (when (plusp (logand allowed (ash 1 id2)))
                                           (let ((ways (if (= id1 id2)
                                                           (* (get-w id1) (get-w id1))
                                                           (* 2 (get-w id1) (get-w id2))))
                                                 (nsum (mod (+ sum (* (+ (get-v id1) (get-v id2)) (aref p3 m))) 7))
                                                 (nforb (logior forb (aref precomp R m id1) (aref precomp R m id2))))
                                             (incf res (* ways (dfs (1+ m) nsum nforb)))))))))
                                   
                                  ((= k-count 3)
                                   (iterate (for id1 from 0 to 7)
                                     (when (plusp (logand allowed (ash 1 id1)))
                                       (iterate (for id2 from id1 to 7)
                                         (when (plusp (logand allowed (ash 1 id2)))
                                           (iterate (for id3 from id2 to 7)
                                             (when (plusp (logand allowed (ash 1 id3)))
                                               (let ((ways (* (get-w id1) (get-w id2) (get-w id3)
                                                              (cond ((and (= id1 id2) (= id2 id3)) 1)
                                                                    ((or (= id1 id2) (= id2 id3)) 3)
                                                                    (t 6))))
                                                     (nsum (mod (+ sum (* (+ (get-v id1) (get-v id2) (get-v id3)) (aref p3 m))) 7))
                                                     (nforb (logior forb (aref precomp R m id1) (aref precomp R m id2) (aref precomp R m id3))))
                                                 (incf res (* ways (dfs (1+ m) nsum nforb))))))))))))
                                 res)))) ; ★ ここが修正箇所：関数定義リストを閉じる5つ目の括弧
                  
                  ;; labelsの本体として正しく認識される
                  (incf ans (dfs 0 (mod (* D (aref p3 M)) 7) init-forb)))))))))
    ans))

(defun main ()
  (format t "C(100) = ~D~%" (solve-p954 2))
  (format t "C(10^4) = ~D~%" (solve-p954 4))
  (format t "C(10^13) = ~D~%" (solve-p954 13)))


#+| Do it | (main )
#|------------------------------------------------------------|
Timing the evaluation of (main)
C(100) = 74
C(10^4) = 3737
C(10^13) = 736463823

User time    =       37.486
System time  =        0.331
Elapsed time =       39.192
Allocation   = 496568 bytes
6496 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ nil
:ok