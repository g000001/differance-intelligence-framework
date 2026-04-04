#include <stdint.h>

// Stern-Brocot木の逆行をユークリッド互除法で高速化するアルゴリズム
int solve_175(int64_t p, int64_t q, int32_t* out) {
    int idx = 0;
    while (p > 0 && q > 0) {
        if (p > q) {
            int64_t k = p / q;
            p = p % q;
            if (p == 0) {
                out[idx++] = (int32_t)(k - 1);
                out[idx++] = 1;
                break;
            } else {
                out[idx++] = (int32_t)k;
            }
        } else if (q > p) {
            int64_t k = q / p;
            q = q % p;
            if (q == 0) {
                out[idx++] = (int32_t)k;
                break;
            } else {
                out[idx++] = (int32_t)k;
            }
        } else {
            out[idx++] = 1;
            break;
        }
    }
    return idx;
}
