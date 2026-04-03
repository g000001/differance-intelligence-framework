// pe181.c
#include <stdint.h>
#include <stdlib.h>

// Lispから呼び出すためのエントリポイント
uint64_t solve_181(int B_max, int W_max) {
    int w_limit = W_max + 1;
    int size = (B_max + 1) * w_limit;
    
    // calloc で 0 初期化された配列を確保
    uint64_t *dp = (uint64_t *)calloc(size, sizeof(uint64_t));
    if (!dp) return 0;
    
    dp[0] = 1;
    
    for (int db = 0; db <= B_max; db++) {
        for (int dw = 0; dw <= W_max; dw++) {
            if (db == 0 && dw == 0) continue;
            
            for (int b = db; b <= B_max; b++) {
                int b_offset = b * w_limit;
                int prev_b_offset = (b - db) * w_limit;
                for (int w = dw; w <= W_max; w++) {
                    int idx = b_offset + w;
                    int prev_idx = prev_b_offset + (w - dw);
                    dp[idx] += dp[prev_idx];
                }
            }
        }
    }
    
    uint64_t ans = dp[size - 1];
    free(dp); // メモリリークを防ぐ
    
    return ans;
}
