
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
