
#include <stdint.h>
#include <stdlib.h>

uint64_t solve_171_core(int digits) {
    // DP arrays: count[i][s] と sum[i][s]
    // i: 処理した桁数 (0 to 20)
    // s: 桁の2乗和 (0 to 1620)
    static uint64_t count[21][1621] = {0};
    static uint64_t sum[21][1621] = {0};
    
    // DP初期化
    for (int i = 0; i <= digits; i++) {
        for (int s = 0; s <= 1620; s++) {
            count[i][s] = 0;
            sum[i][s] = 0;
        }
    }
    
    count[0][0] = 1;
    uint64_t p10 = 1; // 10^i mod 10^9
    
    for (int i = 0; i < digits; i++) {
        for (int s = 0; s <= 1620; s++) {
            if (count[i][s] == 0) continue;
            
            for (int d = 0; d <= 9; d++) {
                int ns = s + d * d;
                if (ns > 1620) continue; // 最大値クリップ
                
                // パターン数の遷移
                count[i+1][ns] = (count[i+1][ns] + count[i][s]) % 1000000000ULL;
                
                // 和の遷移
                // 新しい桁 d は 10^i の位になるため、d * 10^i をパターンの数だけ足す
                uint64_t term = (d * p10) % 1000000000ULL;
                term = (term * count[i][s]) % 1000000000ULL;
                
                sum[i+1][ns] = (sum[i+1][ns] + sum[i][s] + term) % 1000000000ULL;
            }
        }
        p10 = (p10 * 10) % 1000000000ULL;
    }
    
    uint64_t ans = 0;
    // 2乗和が完全平方数になるものの和を合算
    for (int k = 1; k * k <= 1620; k++) {
        ans = (ans + sum[digits][k * k]) % 1000000000ULL;
    }
    
    return ans;
}
