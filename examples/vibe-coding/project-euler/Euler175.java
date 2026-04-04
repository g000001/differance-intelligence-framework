
public class Euler175 {
    public static int solve(long p, long q, int[] out) {
        int idx = 0;
        // Stern-Brocot (Calkin-Wilf) 木の逆行をユークリッド互除法で高速化
        while (p > 0 && q > 0) {
            if (p > q) {
                long k = p / q;
                p = p % q;
                if (p == 0) {
                    out[idx++] = (int)(k - 1);
                    out[idx++] = 1;
                    break;
                } else {
                    out[idx++] = (int)k;
                }
            } else if (q > p) {
                long k = q / p;
                q = q % p;
                if (q == 0) {
                    out[idx++] = (int)k;
                    break;
                } else {
                    out[idx++] = (int)k;
                }
            } else {
                out[idx++] = 1;
                break;
            }
        }
        return idx;
    }
}
