package main

/*
#include <stdint.h>
*/
import "C"
import "unsafe"

type Pair struct { i, j int }
type Signature struct { lo, hi uint64 }

func getPairID(i, j int) int {
    id := 0
    for a := 0; a < i; a++ {
        id += 15 - 2 - a
    }
    id += j - (i + 2)
    return id
}

func getSig(pairs []Pair) Signature {
    var s Signature
    for _, p := range pairs {
        id := getPairID(p.i, p.j)
        if id < 64 {
            s.lo |= (1 << id)
        } else {
            s.hi |= (1 << (id - 64))
        }
    }
    return s
}

//export Solve300_v2
func Solve300_v2(n C.int, outPtr *C.int32_t) C.int {
    length := int(n)
    out := unsafe.Slice((*int32)(unsafe.Pointer(outPtr)), 1<<length)

    var visited [31][31]bool
    var path [15]struct{x, y int}
    var dx = []int{1, 0, -1, 0}
    var dy = []int{0, 1, 0, -1}
    
    uniqueTopos := make(map[Signature][]Pair)
    
    var dfs func(step int, x, y int)
    dfs = func(step int, x, y int) {
        path[step] = struct{x,y int}{x, y}
        visited[x][y] = true
        
        if step == length - 1 {
            var pairs []Pair
            for i := 0; i < length; i++ {
                for j := i + 3; j < length; j += 2 {
                    dx := path[i].x - path[j].x
                    dy := path[i].y - path[j].y
                    if dx*dx + dy*dy == 1 {
                        pairs = append(pairs, Pair{i, j})
                    }
                }
            }
            sig := getSig(pairs)
            if _, ok := uniqueTopos[sig]; !ok {
                uniqueTopos[sig] = pairs
            }
        } else {
            for i := 0; i < 4; i++ {
                nx, ny := x + dx[i], y + dy[i]
                if !visited[nx][ny] {
                    if step == 0 && i != 0 { continue }
                    if step == 1 && (i == 2 || i == 3) { continue }
                    dfs(step+1, nx, ny)
                }
            }
        }
        visited[x][y] = false
    }
    
    dfs(0, 15, 15)

    type TopoMask []uint16
    var topoMaskList []TopoMask
    for _, pairs := range uniqueTopos {
        var tm TopoMask
        for _, p := range pairs {
            tm = append(tm, uint16((1<<p.i) | (1<<p.j)))
        }
        topoMaskList = append(topoMaskList, tm)
    }

    // 全てのH/Pシーケンスに対して評価
    for seq := 0; seq < (1 << length); seq++ {
        maxScore := 0
        seq16 := uint16(seq)
        
        // 1. 空間的接触 (SAW由来)
        for _, tm := range topoMaskList {
            score := 0
            for _, mask := range tm {
                if (seq16 & mask) == mask {
                    score++
                }
            }
            if score > maxScore {
                maxScore = score
            }
        }
        
        // 2. 主鎖(Backbone)の接触 (シーケンス固有)
        backboneScore := 0
        for i := 0; i < length - 1; i++ {
            mask := uint16(3 << i)
            if (seq16 & mask) == mask {
                backboneScore++
            }
        }
        
        out[seq] = int32(maxScore + backboneScore)
    }
    return 0
}

func main() {}
