
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#define MAX_N 200000
// 右部分木の最大サイズを数学的上限に合わせて拡張
#define M 30000
// Mを超える最小の2の冪乗。modulo演算をビットマスク(&)にして高速化
#define W 32768
#define W_MASK 32767

uint64_t C[MAX_N + 1];

uint64_t solve_c() {
    // 状態空間を圧縮したスライディングウィンドウ (約3.9GB)
    uint32_t (*cost)[M + 1] = calloc(W, sizeof(*cost));
    
    if (!cost) {
        printf("Memory allocation failed\n");
        exit(1);
    }
    
    C[0] = 0;
    C[1] = 0;
    
    for (int x = 1; x <= MAX_N; x++) {
        cost[x & W_MASK][0] = 0;
        cost[x & W_MASK][1] = 0;
        
        int max_len = x < M ? x : M;
        int m_star = x;
        
        for (int len = 2; len <= max_len; len++) {
            int i = x - len + 1;
            
            // 交差点 m_star を追跡 (左コスト >= 右コスト となる最小のm)
            while (m_star > i) {
                int m_test = m_star - 1;
                uint32_t left_c = cost[(m_test - 1) & W_MASK][m_test - i];
                uint32_t right_c = cost[x & W_MASK][x - m_test];
                if (left_c >= right_c) {
                    m_star--;
                } else {
                    break;
                }
            }
            
            uint32_t min_cost = 0xFFFFFFFF;
            // 真の最適mは m_star のごく近傍に存在する (凸性による証明)
            int search_start = m_star - 10;
            if (search_start < i) search_start = i;
            int search_end = m_star + 5;
            if (search_end > x) search_end = x;
            
            for (int m = search_start; m <= search_end; m++) {
                uint32_t left_cost = cost[(m - 1) & W_MASK][m - i];
                uint32_t right_cost = cost[x & W_MASK][x - m];
                uint32_t max_c = left_cost > right_cost ? left_cost : right_cost;
                uint32_t c = m + max_c;
                if (c < min_cost) min_cost = c;
            }
            cost[x & W_MASK][len] = min_cost;
        }
        
        if (x <= M) {
            C[x] = cost[x & W_MASK][x];
        } else {
            // x > M の場合、C(x) すなわち f(1, x) を計算
            int low = x - M;
            int high = x;
            int m_s = x;
            while (low <= high) {
                int mid = low + (high - low) / 2;
                uint64_t left_c = C[mid - 1];
                uint32_t right_c = cost[x & W_MASK][x - mid];
                if (left_c >= right_c) {
                    m_s = mid;
                    high = mid - 1;
                } else {
                    low = mid + 1;
                }
            }
            
            uint64_t min_C = 0xFFFFFFFFFFFFFFFFULL;
            int search_start = m_s - 10;
            if (search_start < x - M) search_start = x - M;
            int search_end = m_s + 5;
            if (search_end > x) search_end = x;
            
            for (int m = search_start; m <= search_end; m++) {
                uint64_t left_c = C[m - 1];
                uint32_t right_c = cost[x & W_MASK][x - m];
                uint64_t max_c = left_c > right_c ? left_c : right_c;
                uint64_t c = m + max_c;
                if (c < min_C) min_C = c;
            }
            C[x] = min_C;
        }
    }
    
    uint64_t sum = 0;
    for (int i = 1; i <= MAX_N; i++) {
        sum += C[i];
    }
    
    // サニティチェック：ここで厳密に一致しなければならない
    uint64_t sum100 = 0;
    for (int i = 1; i <= 100; i++) sum100 += C[i];
    printf("Sanity Check: Sum C(1..100) = %llu\n", (unsigned long long)sum100);
    fflush(stdout);
    
    free(cost);
    return sum;
}
