package main

import "C"
import "unsafe"

//export Solve309
func Solve309(limit C.int, headPtr *C.int, nextPtr *C.int, valPtr *C.int, maxNodes C.int) C.int {
    N := int(limit)
    // Lisp側で確保したCポインタをGoのスライスとして安全にキャスト
    head := unsafe.Slice((*int32)(unsafe.Pointer(headPtr)), N)
    next := unsafe.Slice((*int32)(unsafe.Pointer(nextPtr)), int(maxNodes))
    val  := unsafe.Slice((*int32)(unsafe.Pointer(valPtr)), int(maxNodes))

    // リンクドリストのヘッドを初期化 (-1 is null)
    for i := 0; i < N; i++ {
        head[i] = -1
    }

    nodeCount := int32(0)

    // Lispから渡されたメモリ領域にノードを追加 (Zero Allocation)
    addNode := func(w, a int) {
        if w >= N { return }
        if nodeCount >= int32(maxNodes) { return }
        idx := nodeCount
        nodeCount++
        val[idx] = int32(a)
        next[idx] = head[w]
        head[w] = idx
    }

    gcd := func(a, b int) int {
        for b != 0 {
            a, b = b, a%b
        }
        return a
    }

    // 原始ピタゴラス数の生成と倍数の展開
    for m := 2; m*m < N; m++ {
        for n := 1; n < m; n++ {
            if (m-n)%2 == 1 && gcd(m, n) == 1 {
                a0 := m*m - n*n
                b0 := 2 * m * n
                c0 := m*m + n*n
                if c0 >= N { continue }
                
                // 倍数 k を掛けて展開し、斜辺 c < N のものをすべて登録
                for k := 1; k*c0 < N; k++ {
                    addNode(k*a0, k*b0)
                    addNode(k*b0, k*a0)
                }
            }
        }
    }

    count := 0
    var as [4000]int // 各 w に対する a の値を格納するローカルバッファ
    
    // 各 w について、a の組み合わせを評価
    for w := 1; w < N; w++ {
        nA := 0
        for idx := head[w]; idx != -1; idx = next[idx] {
            if nA < 4000 {
                as[nA] = int(val[idx])
                nA++
            }
        }
        
        // ペア (a, b) の検証
        for i := 0; i < nA; i++ {
            a := as[i]
            for j := i + 1; j < nA; j++ {
                b := as[j]
                // a * b が a + b で割り切れるか
                if int64(a)*int64(b)%int64(a+b) == 0 {
                    count++
                }
            }
        }
    }
    return C.int(count)
}

func main() {}
