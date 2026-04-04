#import <Foundation/Foundation.h>
#include <math.h>
#include <stdbool.h>

typedef __int128 i128;

static long long gcd(long long a, long long b) {
    while (b) { long long r = a % b; a = b; b = r; }
    return a;
}

static bool is_le(long long Y, long long n, long long P, long long Q, long long R, long long D) {
    i128 L = (i128)Y * R - (i128)n * Q;
    i128 Rhs = (i128)n * P;

    if (Rhs == 0) return L <= 0;
    if (Rhs > 0) {
        if (L <= 0) return true;
        return L * L <= Rhs * Rhs * D;
    } else {
        if (L >= 0) return false;
        return L * L >= Rhs * Rhs * D;
    }
}

static long long calc(long long n, long long P, long long Q, long long R, long long D) {
    if (n == 0) return 0;
    long double sqD = sqrtl(D);
    long long m = (long long)((P * sqD + Q) / R);

    while (is_le(m + 1, 1, P, Q, R, D)) m++;
    while (!is_le(m, 1, P, Q, R, D)) m--;

    if (m != 0) {
        return m * n * (n + 1) / 2 + calc(n, P, Q - m * R, R, D);
    }

    long long Y = (long long)(n * ((P * sqD + Q) / R));
    while (is_le(Y + 1, n, P, Q, R, D)) Y++;
    while (!is_le(Y, n, P, Q, R, D)) Y--;

    if (Y == 0) return 0;

    long long P_new = P * R;
    long long Q_new = -Q * R;
    long long R_new = P * P * D - Q * Q;

    long long g1 = gcd(llabs(P_new), llabs(Q_new));
    long long g = gcd(g1, llabs(R_new));
    P_new /= g;
    Q_new /= g;
    R_new /= g;

    if (R_new < 0) {
        P_new = -P_new;
        Q_new = -Q_new;
        R_new = -R_new;
    }

    return n * Y - calc(Y, P_new, Q_new, R_new, D);
}

static long long isqrt_exact(long long n) {
    long long s = (long long)sqrt((double)n);
    while ((s + 1) * (s + 1) <= n) s++;
    while (s * s > n) s--;
    return s;
}

static long long S_func(long long A, long long B, long long k) {
    if (A > B) return 0;
    long long r = isqrt_exact(k);
    if (r * r == k) {
        return r * (A + B) * (B - A + 1) / 2;
    } else {
        long long GB = calc(B, 1, 0, 1, k);
        long long GA = calc(A - 1, 1, 0, 1, k);
        return GB - GA + (B - A + 1);
    }
}

static long long get_x_max(long long N, long long k) {
    long long x = (long long)(N / sqrt((double)k));
    while ((x + 1) * (x + 1) * k <= N * N) x++;
    while (x * x * k > N * N) x--;
    return x;
}

@interface Euler372 : NSObject
// CFFIの型衝突を避けるため、ポインタを void * で受け取る
+ (void)solveWithM:(long long)M N:(long long)N out:(void *)out_ptr;
@end

@implementation Euler372
+ (void)solveWithM:(long long)M N:(long long)N out:(void *)out_ptr {
    // Objective-C側で元の long long * にキャスト
    long long *out_array = (long long *)out_ptr;
    long long total = 0;
    
    long long K_max = (N * N) / ((M + 1) * (M + 1));
    
    for (long long k = 1; k <= K_max; k += 2) {
        long long x_max_k = get_x_max(N, k);
        long long x_max_kp1 = get_x_max(N, k + 1);
        
        long long f_k = S_func(M + 1, x_max_k, k);
        long long f_kp1 = S_func(M + 1, x_max_kp1, k + 1);
        
        total += f_kp1 - f_k + (x_max_k - x_max_kp1) * (N + 1);
    }
    
    out_array[0] = total;
}
@end
