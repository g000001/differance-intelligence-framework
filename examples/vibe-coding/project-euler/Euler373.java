
public class Euler373 {
    // Lisp側で確保された共有メモリ (out) に結果を書き込む
    public static void solve(long N_long, long[] out) {
        int N = (int)N_long;
        
        // 状態空間の次元崩壊: DP配列 (約120MB)
        int[] min_p = new int[N + 1];
        int[] N1 = new int[N + 1];
        int[] N2 = new int[N + 1];
        
        N1[1] = 1;
        N2[1] = 1;
        
        // エラトステネスの篩による最小素因数の記録: O(N log log N)
        for (int i = 2; i <= N; i++) {
            if (min_p[i] == 0) {
                for (int j = i; j <= N; j += i) {
                    if (min_p[j] == 0) min_p[j] = i;
                }
            }
        }
        
        long ans = 0;
        
        // Burnsideの補題とガウス整数に基づくO(N)の軌道数え上げ
        for (int i = 2; i <= N; i++) {
            int p = min_p[i];
            int temp = i;
            int a = 0;
            
            // 最小素因数pの指数aをカウントし、残りの部分(temp)と分離
            while (temp % p == 0) {
                temp /= p;
                a++;
            }
            
            // p ≡ 1 (mod 4) の素因数のみが、三角形の自由度（ピタゴラスの角）を生み出す
            if (p % 4 == 1) {
                N1[i] = N1[temp] * (3 * a * a + 3 * a + 1);
                N2[i] = N2[temp] * (2 * (a / 2) + 1);
            } else {
                // p ≡ 3 (mod 4) または p = 2 は自由度に寄与しない
                N1[i] = N1[temp];
                N2[i] = N2[temp];
            }
            
            // 軌道の数 (有効な三角形の数) を定数時間で導出
            long c = (N1[i] + 3L * N2[i] - 4L) / 6L;
            if (c > 0) {
                ans += (long)i * c; // 半径 i を三角形の数だけ掛け合わせて総和
            }
        }
        out[0] = ans;
    }
}
