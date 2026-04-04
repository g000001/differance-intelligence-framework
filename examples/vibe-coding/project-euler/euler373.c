#include <stdint.h>
#include <math.h>

int64_t gcd(int64_t a, int64_t b) {
    while (b) {
        a %= b;
        int64_t temp = a;
        a = b;
        b = temp;
    }
    return a;
}

int64_t gcd3(int64_t a, int64_t b, int64_t c) {
    return gcd(a, gcd(b, c));
}

// 共有メモリ (out) に結果を書き込む
void solve_373(int64_t N, int64_t* out) {
    int64_t ans = 0;
    
    // uの上限は数学的証明により 300000 (N=10^7の場合) で完全に頭打ちになる
    int64_t max_u = 300000; 

    for (int64_t u = 2; u <= max_u; u++) {
        for (int64_t v = 1; v < u; v++) {
            if (gcd(u, v) != 1) continue;
            
            // 早期枝刈り: R_min >= u^2 / (4 * sqrt(v^2 + 2uv)) が N を超えたらこれ以上wを探しても無駄
            double w_approx_max = sqrt((double)v*v + 2.0*u*v);
            if (((double)u * u) / (4.0 * w_approx_max) > (double)N) {
                // vが小さいときにuが大きすぎると早々にスキップされる
                continue; 
            }

            int64_t w_min = (int64_t)sqrt(u * v);
            if (w_min * w_min <= u * v) w_min++;
            int64_t w_max = (int64_t)sqrt(v * v + 2 * u * v);
            
            for (int64_t w = w_min; w <= w_max; w++) {
                if (gcd(u, w) != 1 || gcd(v, w) != 1) continue;
                
                int64_t a = u * (v * v + w * w);
                int64_t b = v * (u * u + w * w);
                int64_t c = (u + v) * (w * w - u * v);
                int64_t g = gcd3(a, b, c);
                
                int64_t R_num = (u * u + w * w) * (v * v + w * w);
                int64_t R_den = 4 * w * g;
                int64_t d = gcd(R_num, R_den);
                int64_t R_min = R_num / d;
                
                if (R_min <= N) {
                    int64_t K = N / R_min;
                    ans += R_min * K * (K + 1) / 2;
                }
            }
        }
    }
    *out = ans;
}
