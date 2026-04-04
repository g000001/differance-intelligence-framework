;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3.1-pro
(cl:in-package cl-user)
(defpackage #:project-euler-0334 (:use cl) (:export #:solve))
(in-package #:project-euler-0334)

(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :uiop)
  #+lispworks (require "java-interface"))

;; macOS環境等で成功したJVMのパスを指定
(defvar *jvm-library-path*
  (or (uiop:getenv "JAVA_HOME")
      "/Library/Java/JavaVirtualMachines/jdk-20.jdk/Contents/Home/lib/server/libjvm.dylib"))

;; Lispコンパイル時にJavaの静的メソッドをLisp関数としてバインド
#+lispworks
(lw-ji:define-java-caller %euler-334-solve "Euler334" "solve")

;;; ----------------------------------------------------------------------
;;; Lisp 実行関数
;;; ----------------------------------------------------------------------
(defun solve ()
  "Find the number of moves to finish the game with 1500 bowls."
  
  (let* ((dir (uiop:pathname-directory-pathname (source-pathname)))
         (java-file (merge-pathnames "Euler334.java" dir)))
    
    (format t "Generating Java source code...~%")
    (with-open-file (out java-file :direction :output :if-exists :supersede)
      (write-string "
import java.math.BigInteger;

public class Euler334 {
    
    // 安全な切り捨て除算 (Java 8 Math.floorDiv 相当)
    private static long floorDiv(long x, long y) {
        long r = x / y;
        if ((x ^ y) < 0 && (r * y != x)) {
            r--;
        }
        return r;
    }

    // 1^2 + 2^2 + ... + n^2 の計算 (オーバーフローを完全に防ぐため BigInteger を使用)
    public static BigInteger sumSq(long n) {
        if (n < 0) return sumSq(-n);
        if (n == 0) return BigInteger.ZERO;
        BigInteger bn = BigInteger.valueOf(n);
        return bn.multiply(bn.add(BigInteger.ONE))
                 .multiply(bn.multiply(BigInteger.valueOf(2)).add(BigInteger.ONE))
                 .divide(BigInteger.valueOf(6));
    }

    // A^2 + (A+1)^2 + ... + B^2 の計算
    public static BigInteger sumSqRange(long A, long B) {
        if (A > B) return BigInteger.ZERO;
        if (A >= 0) {
            return sumSq(B).subtract(sumSq(A - 1));
        } else if (B <= 0) {
            return sumSq(-A).subtract(sumSq(-B - 1));
        } else {
            return sumSq(-A).add(sumSq(B));
        }
    }

    // Lisp側で確保された共有メモリ (out) に結果を書き込む
    public static void solve(long[] out) {
        long t = 123456;
        long N = 0;
        long M1 = 0;
        BigInteger S_initial = BigInteger.ZERO;

        // 1. 初期状態の生成と不変量の計算
        for (long i = 1; i <= 1500; i++) {
            if (t % 2 == 0) {
                t = t / 2;
            } else {
                t = (t / 2) ^ 926252;
            }
            long b = (t % 2048) + 1;
            
            N += b;
            M1 += i * b;
            S_initial = S_initial.add(BigInteger.valueOf(i * i * b));
        }

        BigInteger S_final = BigInteger.ZERO;
        
        // 2. 最終状態の数学的決定 (O(1) 次元崩壊)
        long n_choose_2 = N * (N - 1) / 2;
        long K = M1 - n_choose_2;

        if (K % N == 0) {
            // 穴が0個の完全な区間
            long L = K / N;
            S_final = sumSqRange(L, L + N - 1);
        } else {
            // 穴が1つだけ存在する区間
            long n_plus_1_choose_2 = N * (N + 1) / 2;
            long K2 = M1 - n_plus_1_choose_2;
            long L = floorDiv(K2, N) + 1;
            long h = (N + 1) * L + n_plus_1_choose_2 - M1; // 穴の位置
            
            S_final = sumSqRange(L, L + N).subtract(BigInteger.valueOf(h * h));
        }

        // 3. 不変量に基づく総移動回数の計算
        BigInteger moves = S_final.subtract(S_initial).divide(BigInteger.valueOf(2));
        out[0] = moves.longValue();
    }
}
" out))
    
    (format t "Compiling Java code...~%")
    (uiop:run-program (list "javac" (namestring java-file)))
    
    #+lispworks
    (progn
      (format t "Initializing JVM...~%")
      (lw-ji:init-java-interface :jvm-library-path *jvm-library-path* :java-class-path (namestring dir))
      
      (format t "Allocating shared memory in Common Lisp...~%")
      ;; 共有メモリの確保: Lisp側でJavaのlong配列をアロケート (結果格納用)
      (let ((out-array (lw-ji:make-java-array :long 1)))
        
        (format t "Executing highly optimized math shortcut in Java via define-java-caller...~%")
        (time (%euler-334-solve out-array))
        
        ;; Lisp側で共有メモリから結果を読み出す
        (let ((ans (lw-ji:jvref out-array 0)))
          (format t "Moves required = ~D~%" ans)
          ans)))))

#+| Do it | (solve )
#|
Timing the evaluation of (project-euler-0334:solve)
Generating Java source code...
Compiling Java code...
Initializing JVM...
Allocating shared memory in Common Lisp...
Executing highly optimized math shortcut in Java via define-java-caller...
Timing the evaluation of (project-euler-0334::%euler-334-solve project-euler-0334::out-array)

User time    =        0.003
System time  =        0.000
Elapsed time =        0.003
Allocation   = 8896 bytes
275 Page faults
GC time      =        0.000
Moves required = 150320021261690835
;;→ 150320021261690835
|#
:ok