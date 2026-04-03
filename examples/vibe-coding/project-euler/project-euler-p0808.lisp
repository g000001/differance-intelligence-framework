;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0808 (:use cl) (:export #:solve))
(in-package #:project-euler-0808)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi)
  (ql:quickload :uiop))

;; Swiftのコンパイル: -emit-library を使用
(let* ((base-dir (uiop:pathname-directory-pathname (source-pathname)))
       (swift-file (merge-pathnames "project-euler-p0808.swift" base-dir))
       (dylib-file (merge-pathnames "libeuler808.dylib" base-dir)))
  (unless (uiop:file-exists-p dylib-file)
    (uiop:run-program 
     (list "swiftc" "-O" "-emit-library" "-o" (namestring dylib-file) (namestring swift-file))))
  (cffi:load-foreign-library dylib-file))

;; CFFIインターフェース定義
(cffi:defcfun ("check_reversible_prime_square" %check-reversible-prime-square) :int32
  (p :uint64)
  (sieve-ptr :pointer)
  (sieve-size :uint64))

(defun solve ()
  (let* ((target-count 50)
         (limit-p #.(expt 10 8)) ; 探索範囲
         (sieve-size limit-p)
         (count 0)
         (sum 0))
    (format t "Allocating sieve of size ~A...~%" sieve-size)
    ;; C側（Swift側）から高速参照できる共有メモリとして篩を作成
    (cffi:with-foreign-object (sieve-ptr :uint8 sieve-size)
      ;; 篩の初期化 (1で素数、0で非素数)
      (loop for i from 0 below sieve-size do (setf (cffi:mem-aref sieve-ptr :uint8 i) 1))
      (setf (cffi:mem-aref sieve-ptr :uint8 0) 0)
      (setf (cffi:mem-aref sieve-ptr :uint8 1) 0)
      
      (format t "Starting Sieve and Search...~%")
      (loop for p from 2 below limit-p
            when (= 1 (cffi:mem-aref sieve-ptr :uint8 p))
            do (progn
                 ;; Swiftによる判定
                 (when (= 1 (%check-reversible-prime-square p sieve-ptr sieve-size))
                   (incf count)
                   (let ((val (* p p)))
                     (incf sum val)
                     (format t "[~D] Found: ~D (p=~D)~%" count val p)))
                 
                 ;; 篩の更新
                 (loop for i from (* p p) below limit-p by p
                       do (setf (cffi:mem-aref sieve-ptr :uint8 i) 0))
                 
                 (when (>= count target-count)
                   (return-from solve sum))))
    sum)))


#+| Do it | (SOLVE )
#|------------------------------------------------------------|
Timing the evaluation of (SOLVE)
Allocating sieve of size 100000000...
Starting Sieve and Search...
[1] Found: 169 (p=13)
[2] Found: 961 (p=31)
[3] Found: 12769 (p=113)
[4] Found: 96721 (p=311)
[5] Found: 1042441 (p=1021)
[6] Found: 1062961 (p=1031)
[7] Found: 1216609 (p=1103)
[8] Found: 1442401 (p=1201)
[9] Found: 1692601 (p=1301)
[10] Found: 9066121 (p=3011)
[11] Found: 121066009 (p=11003)
[12] Found: 900660121 (p=30011)
[13] Found: 12148668841 (p=110221)
[14] Found: 12367886521 (p=111211)
[15] Found: 12568876321 (p=112111)
[16] Found: 14886684121 (p=122011)
[17] Found: 1000422044521 (p=1000211)
[18] Found: 1002007006009 (p=1001003)
[19] Found: 1020506060401 (p=1010201)
[20] Found: 1040606050201 (p=1020101)
[21] Found: 1210684296721 (p=1100311)
[22] Found: 1212427816609 (p=1101103)
[23] Found: 1212665666521 (p=1101211)
[24] Found: 1214648656321 (p=1102111)
[25] Found: 1234367662441 (p=1111021)
[26] Found: 1236568464121 (p=1112011)
[27] Found: 1254402240001 (p=1120001)
[28] Found: 1256665662121 (p=1121011)
[29] Found: 1276924860121 (p=1130011)
[30] Found: 1442667634321 (p=1201111)
[31] Found: 9006007002001 (p=3001001)
[32] Found: 9066187242121 (p=3011011)
[33] Found: 100042424498641 (p=10002121)
[34] Found: 100222143232201 (p=10011101)
[35] Found: 100240164024001 (p=10012001)
[36] Found: 100402824854641 (p=10020121)
[37] Found: 100420461042001 (p=10021001)
[38] Found: 102012282612769 (p=10100113)
[39] Found: 102014060240401 (p=10100201)
[40] Found: 102232341222001 (p=10111001)
[41] Found: 104042060410201 (p=10200101)
[42] Found: 121002486012769 (p=11000113)
[43] Found: 121264386264121 (p=11012011)
[44] Found: 121462683462121 (p=11021011)
[45] Found: 123212686214641 (p=11100121)
[46] Found: 146412686212321 (p=12100111)
[47] Found: 146458428204001 (p=12102001)
[48] Found: 146894424240001 (p=12120001)
[49] Found: 967210684200121 (p=31100011)
[50] Found: 967216282210201 (p=31100101)

User time    =        2.225
System time  =        0.039
Elapsed time =        2.198
Allocation   = 392648 bytes
3747 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 3807504276997394
:ok