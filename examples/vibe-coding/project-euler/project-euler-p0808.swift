import Foundation

// 数値演算による逆順化 (Stringを経由しない)
func reverseInt(_ n: UInt64) -> UInt64 {
    var num = n
    var rev: UInt64 = 0
    while num > 0 {
        rev = rev * 10 + (num % 10)
        num /= 10
    }
    return rev
}

@_cdecl("check_reversible_prime_square")
public func checkReversiblePrimeSquare(p: UInt64, sievePtr: UnsafePointer<UInt8>, sieveSize: UInt64) -> Int32 {
    let pSquared = p * p
    let revPSquared = reverseInt(pSquared)
    
    // 1. 回文数チェック
    if pSquared == revPSquared { return 0 }
    
    // 2. 逆順が平方数かチェック
    let root = UInt64(round(sqrt(Double(revPSquared))))
    if root * root != revPSquared { return 0 }
    
    // 3. その平方根が素数かチェック (Lisp側から渡された篩を利用)
    if root < sieveSize {
        return sievePtr[Int(root)] == 1 ? 1 : 0
    } else {
        // 篩の範囲外（念のためのフォールバック）
        if root % 2 == 0 { return 0 }
        var i: UInt64 = 3
        while i * i <= root {
            if root % i == 0 { return 0 }
            i += 2
        }
        return 1
    }
}
