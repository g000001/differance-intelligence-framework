%% 提示されたゼブラ問題のコードに対して、視点数付き寓（PoV-Allegory）のモジュラー律 $RS \cap T \subseteq R(S \cap R^\circ T)$ に基づく「未来の引き戻し」を適用した全体プログラムを以下に示す。

%% 大域的なバックトラック（継続残余 $\rho_{\text{left}}$）の要因となる未来の制約 $T$（ノルウェー人の隣の家が青であること）を、逆関係 $R^\circ$ を用いて初期状態 $R$ に引き戻し、コンパイル時における局所的な構造制約として統合している。

%% 
/* -*- Mode: prolog -*-
 *
 * This file for benchmarking against SWI Prolog.
 */

nextto(X, Y, List) :- iright(X, Y, List).
nextto(X, Y, List) :- iright(Y, X, List).
iright(Left, Right, [Left, Right | _]).
iright(Left, Right, [_ | Rest]) :- iright(Left, Right, Rest).


zebra(H, W, Z) :-
    /* 
     * 未来の引き戻しの適用（モジュラー律 $R(S \cap R^\circ T)$）
     * 
     * 初期状態 $R$（ノルウェー人が1軒目、ミルクが3軒目）に対し、
     * 未来の制約 $T$ : nextto(house(norwegian, _, _, _, _), house(_, _, _, _, blue), H)
     * を逆関係 $R^\circ$ を介して手前へ引き戻し、中間プロセス $S$ が展開される前に統合する。
     * ノルウェー人が1軒目であるため、隣接する青い家は演繹的に2軒目に確定する。
     */
    H = [house(norwegian, _, _, _, _), house(_, _, _, _, blue), house(_, _, _, milk, _), _, _],
    
    % 中間プロセス $S$ の展開
    member(house(englishman, _, _, _, red), H),
    member(house(spaniard, dog, _, _, _), H),
    member(house(_, _, _, coffee, green), H),
    member(house(ukrainian, _,  _, tea, _), H),
    iright(house(_, _, _, _, ivory), house(_, _, _, _, green), H),
    member(house(_, snails, winston, _, _), H),
    member(house(_, _, kools, _, yellow), H),
    nextto(house(_, _, chesterfield, _, _), house(_, fox, _, _, _), H),
    nextto(house(_, _, kools, _, _), house(_, horse, _, _, _), H),
    member(house(_, _, luckystrike, oj, _), H),
    member(house(japanese, _, parliaments, _, _), H),
    
    /* 
     * 未来の制約 $T$ は初期状態 $R$ へ既に引き戻され（局所化され）たため、
     * 大域的なバックトラックを誘発する以下の節は削除可能となる。
     * % nextto(house(norwegian, _, _, _, _), house(_, _, _, _, blue), H), 
     */
    
    member(house(W, _, _, water, _), H),
    member(house(Z, zebra, _, _, _), H).

/* This runs the query a single time:
 *  ?- zebra(Houses, WaterDrinker, ZebraOwner).
 */

zebra1(Houses, WaterDrinker, ZebraOwner) :-
        zebra(Houses, WaterDrinker, ZebraOwner), !.

benchmark1 :-
        flag(benchmark,_,0),
        repeat,
        zebra1(Houses, WaterDrinker, ZebraOwner),
        flag(benchmark,N,N+1),
        N = 1000,
        !.

benchmark :- time(benchmark1).


%% ### 記号的最適化の意義
%% このコード変形により、元のプログラムが抱えていた「探索木の深部で `nextto(norwegian, blue)` の評価に失敗し、大域的にバックトラックする」という継続残余 $\rho_{\text{left}}$ の増大を回避している。2軒目の家に青以外の属性（例：赤や緑）を割り当てようとする無効な探索枝は、初期状態 $R$ の段階で型エラー（単一化の不整合）として即座に刈り込まれるため、計算コストは局所的残余 $\rho_{\text{right}}$ へと大幅に圧縮される。
