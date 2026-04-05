;;; -*- mode: Lisp; coding: utf-8  -*-
;;; llm-model: gemini-3-flash-preview
(cl:in-package cl-user)
(defpackage #:project-euler-0331 (:use cl iterate alexandria) (:export #:solve))
(in-package #:project-euler-0331)

(defmacro optimized-code-p (boole)
  (typecase boole
    (null nil)
    (T `(declaim (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))))))

(optimized-code-p nil)


(defmacro source-pathname ()
  "Compute source pathname"
  `(load-time-value ,(or *compile-file-truename* *load-truename* (uiop:getcwd))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+quicklisp (ql:quickload :cffi :silent t))

(defun build-and-load-c-code ()
  (let* ((base-dir (source-pathname))
         (c-file (merge-pathnames "pe331_solver.c" base-dir))
         (so-file (merge-pathnames "pe331_solver.so" base-dir)))
    (with-open-file (out c-file :direction :output :if-exists :supersede)
      (write-string "
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

uint64_t solve_N(uint64_t N) {
    if (N == 5) return 3;
    if (N % 2 != 0) return 0; // 奇数N(>5)は不可能性定理により常に0

    // ビット配列でD_x (各行の初期黒セル数のパリティ) をO(N)メモリで保持
    uint32_t *D_array = calloc((N / 32) + 1, sizeof(uint32_t));
    if (!D_array) {
        printf(\"Memory allocation failed for N=%llu\\n\", (unsigned long long)N);
        return 0;
    }

    uint64_t N2_1 = N * N - 1;
    uint64_t Nm1_2 = (N - 1) * (N - 1);

    uint64_t y_max = N - 1;
    uint64_t y_min = N - 1;

    uint64_t S_A_exact = 0;
    uint64_t sum_C_D0 = 0;
    uint64_t sum_C_D1 = 0;
    uint64_t cnt_D = 0;

    // パス1: 区間サイズの算出とパリティの決定
    for (uint64_t x = 0; x < N; x++) {
        uint64_t x2 = x * x;
        while (y_max > 0 && x2 + y_max * y_max > N2_1) y_max--;
        while (y_min > 0 && x2 + (y_min - 1) * (y_min - 1) >= Nm1_2) y_min--;

        uint64_t count = y_max - y_min + 1;
        S_A_exact += count;
        if (count % 2 == 1) {
            D_array[x / 32] |= (1U << (x % 32));
            cnt_D++;
            sum_C_D1 += count;
        } else {
            sum_C_D0 += count;
        }
    }

    uint64_t S_A = S_A_exact % 2;
    uint64_t S_r = (S_A == 0) ? cnt_D : (N - cnt_D);
    uint64_t sum_rC = (S_A == 0) ? sum_C_D1 : sum_C_D0;

    // パス2: W = Sum(A_{x,y} * r_x * c_y) のスライディングウィンドウ算出
    uint64_t W = 0;
    uint64_t curr_y_max = N - 1;
    uint64_t curr_y_min = N - 1;
    uint32_t dy0 = (D_array[(N - 1) / 32] >> ((N - 1) % 32)) & 1;
    int64_t sum_c = dy0 ^ S_A;

    for (uint64_t x = 0; x < N; x++) {
        uint64_t x2 = x * x;
        uint64_t target_y_max = curr_y_max;
        while (target_y_max > 0 && x2 + target_y_max * target_y_max > N2_1) target_y_max--;
        
        uint64_t target_y_min = curr_y_min;
        while (target_y_min > 0 && x2 + (target_y_min - 1) * (target_y_min - 1) >= Nm1_2) target_y_min--;

        while (curr_y_max > target_y_max) {
            uint32_t dy = (D_array[curr_y_max / 32] >> (curr_y_max % 32)) & 1;
            sum_c -= (dy ^ S_A);
            curr_y_max--;
        }

        while (curr_y_min > target_y_min) {
            curr_y_min--;
            uint32_t dy = (D_array[curr_y_min / 32] >> (curr_y_min % 32)) & 1;
            sum_c += (dy ^ S_A);
        }

        uint32_t dx = (D_array[x / 32] >> (x % 32)) & 1;
        uint64_t rx = dx ^ S_A;
        if (rx == 1) {
            W += sum_c;
        }
    }

    free(D_array);

    // QUBO的定式化に基づく最終コストのO(1)統合
    int64_t total = (int64_t)S_A_exact + 2LL * N * S_r - 4LL * sum_rC - 2LL * S_r * S_r + 4LL * W;
    return (uint64_t)total;
}

uint64_t solve_all() {
    uint64_t total_sum = 3; // i=3 (N=5) の既知解
    for (int i = 4; i <= 31; i += 2) {
        uint64_t N = (1ULL << i) - i;
        uint64_t ans = solve_N(N);
        total_sum += ans;
        printf(\"Computed for i=%d, N=%llu, T=%llu\\n\", i, (unsigned long long)N, (unsigned long long)ans);
        fflush(stdout);
    }
    return total_sum;
}
" out))
    (uiop:run-program (format nil "cc -O3 -shared -fPIC ~A -o ~A" (namestring c-file) (namestring so-file)))
    (cffi:load-foreign-library so-file)))

(build-and-load-c-code)
(cffi:defcfun ("solve_all" solve-all) :uint64)

(defun solve ()
  (format t "Starting PE 331...~%")
  (let ((ans (solve-all)))
    (format t "Done. The answer is: ~A~%" ans)
    ans))


#+| Do it | (solve )
#|------------------------------------------------------------|
Timing the evaluation of (solve)
Starting PE 331...
Done. The answer is: 467178235146843549

User time    =       13.144
System time  =        0.147
Elapsed time =       13.243
Allocation   = 180400 bytes
45041 Page faults
GC time      =        0.000
 |------------------------------------------------------------|#
;;→ 467178235146843549
:ok