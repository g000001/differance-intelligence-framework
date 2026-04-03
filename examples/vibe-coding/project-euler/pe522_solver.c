
#include <stdint.h>
#include <stdlib.h>

uint64_t power_mod(uint64_t base, uint64_t exp, uint64_t mod) {
    uint64_t res = 1;
    base %= mod;
    while (exp > 0) {
        if (exp % 2 == 1) res = (res * base) % mod;
        base = (base * base) % mod;
        exp /= 2;
    }
    return res;
}

uint64_t solve_522(uint64_t n, uint64_t M) {
    // 葉の総和の寄与
    uint64_t L_sum = (n * (n - 1)) % M;
    L_sum = (L_sum * power_mod(n - 2, n - 1, M)) % M;
    
    uint64_t sum_C0 = 0;
    uint64_t P_k = n;
    
    // O(N) 線形時間モジュラ逆元テーブルの構築 (Mが素数であることを前提)
    uint32_t *inv = (uint32_t *)malloc((n + 1) * sizeof(uint32_t));
    if (!inv) return 0;
    inv[1] = 1;
    for (uint32_t i = 2; i <= n; i++) {
        inv[i] = (uint64_t)(M - M / i) * inv[M % i] % M;
    }
    
    // k = 2 ... n-2 までの和
    for (uint64_t k = 2; k <= n - 2; k++) {
        P_k = (P_k * (n - k + 1)) % M;
        uint64_t inv_k = inv[k];
        
        uint64_t m = n - k;
        uint64_t A_m = power_mod(m - 1, m, M);
        
        uint64_t term = (P_k * inv_k) % M;
        term = (term * A_m) % M;
        
        sum_C0 = (sum_C0 + term) % M;
    }
    
    free(inv);
    
    return (L_sum + sum_C0) % M;
}
