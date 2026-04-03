
#include <stdint.h>
#include <math.h>
#include <stdlib.h>

// 拡張ユークリッド互除法
int64_t extgcd(int64_t a, int64_t b, int64_t *x, int64_t *y) {
    int64_t x0 = 1, y0 = 0, x1 = 0, y1 = 1;
    int64_t a_orig = a, b_orig = b;
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    while (b != 0) {
        int64_t q = a / b;
        int64_t r = a % b;
        a = b; b = r;
        int64_t x2 = x0 - q * x1;
        int64_t y2 = y0 - q * y1;
        x0 = x1; x1 = x2;
        y0 = y1; y1 = y2;
    }
    *x = (a_orig < 0) ? -x0 : x0;
    *y = (b_orig < 0) ? -y0 : y0;
    return a;
}

long double solve_core(int64_t r) {
    long double max_p = 0.0;
    long double best_T = 0.0;
    int64_t r_sq = r * r;

    // Ax を 0 から r まで走査し、Ay を ± 両方探ることで、
    // 第1〜第4象限にまたがるすべての直径（スロープ）を完全に網羅する
    for (int64_t Ax = r; Ax >= 0; Ax--) {
        int64_t max_Ay = (int64_t)sqrt((double)(r_sq - Ax * Ax));
        
        for (int64_t abs_Ay = max_Ay; abs_Ay >= max_Ay - 5 && abs_Ay >= 0; abs_Ay--) {
            for (int sign = -1; sign <= 1; sign += 2) {
                int64_t Ay = abs_Ay * sign;
                if (Ay == 0 && sign == -1) continue; // 0の二重カウント防止
                
                // B は A の正反対付近に位置する
                for (int64_t u = -25; u <= 25; u++) {
                    for (int64_t v = -25; v <= 25; v++) {
                        int64_t Bx = -Ax + u;
                        int64_t By = -Ay + v;
                        
                        if (Bx * Bx + By * By > r_sq) continue;
                        
                        int64_t X = Bx - Ax;
                        int64_t Y = By - Ay;
                        
                        if (X == 0 && Y == 0) continue;
                        
                        int64_t dist_sq = X * X + Y * Y;
                        
                        // 【超高速 Pruning】 sqrtl を呼ばずに O(1) で刈り込む
                        // p_max は |AB| の2倍をわずかに超える程度。
                        // したがって 2*|AB| + 100 < p_max なら計算の価値なし。
                        if (max_p > 100.0) {
                            long double t = max_p - 100.0;
                            if (4.0 * (long double)dist_sq < t * t) continue;
                        }
                        
                        long double c = sqrtl((long double)dist_sq);
                        
                        int64_t X_ext, Y_ext;
                        int64_t g = extgcd(X, Y, &X_ext, &Y_ext);
                        if (g != 1 && g != -1) continue;
                        
                        int64_t E = Ax * Y - Ay * X;
                        int64_t targets[2] = {E + 1, E - 1};
                        
                        for (int k_idx = 0; k_idx < 2; k_idx++) {
                            int64_t target = targets[k_idx];
                            
                            // 【オーバーフローの完全排除】
                            __int128 target_128 = (__int128)target;
                            __int128 base_Cx = (__int128)Y_ext * (target_128 / g);
                            __int128 base_Cy = (__int128)(-X_ext) * (target_128 / g);
                            
                            __int128 dot = base_Cx * X + base_Cy * Y;
                            __int128 norm_sq = (__int128)X * X + (__int128)Y * Y;
                            
                            // 原点に最も近い解へのシフト (厳密な 128bit 除算)
                            __int128 num = -dot;
                            __int128 den = norm_sq;
                            int64_t k_shift;
                            if (den < 0) { num = -num; den = -den; }
                            if (num >= 0) {
                                k_shift = (int64_t)((num + den / 2) / den);
                            } else {
                                k_shift = (int64_t)((num - den / 2) / den);
                            }
                            
                            int64_t cx0 = (int64_t)(base_Cx + (__int128)k_shift * X);
                            int64_t cy0 = (int64_t)(base_Cy + (__int128)k_shift * Y);
                            
                            // 最適な C を探索
                            for (int64_t dk = -5; dk <= 5; dk++) {
                                int64_t cx = cx0 + dk * X;
                                int64_t cy = cy0 + dk * Y;
                                
                                if (cx * cx + cy * cy > r_sq) continue;
                                
                                int64_t a_sq = (Bx - cx)*(Bx - cx) + (By - cy)*(By - cy);
                                int64_t b_sq = (Ax - cx)*(Ax - cx) + (Ay - cy)*(Ay - cy);
                                
                                long double p = sqrtl(a_sq) + sqrtl(b_sq) + c;
                                if (p > max_p) {
                                    max_p = p;
                                    long double R_val = sqrtl(a_sq) * sqrtl(b_sq) * c / 2.0;
                                    best_T = R_val / r;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return best_T;
}

double solve_562_test(int64_t r) {
    return (double)solve_core(r);
}

uint64_t solve_562_final(int64_t r) {
    return (uint64_t)roundl(solve_core(r));
}
