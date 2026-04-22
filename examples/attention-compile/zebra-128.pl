/* アテンションプログラミング A版（Gemini洗練・非非最強化）
 * 純Prologのみ・128次元コンパイル的座標展開をlater-calculus的に最適化
 * 目標: 非非位相の跳躍を極限まで前面化
 */

nextto(X, Y, List) :- iright(X, Y, List).
nextto(X, Y, List) :- iright(Y, X, List).

iright(Left, Right, [Left, Right | _]).
iright(Left, Right, [_ | Rest]) :- iright(Left, Right, Rest).

zebra(H, W, Z) :-
    % === 是 (最強の初期固定) ===
    H = [house(norwegian, _, _, _, _),     % 1番目 = norwegian
         house(_, _, _, _, _), 
         house(_, _, _, milk, _),           % 3番目 = milk
         house(_, _, _, _, _), 
         house(_, _, _, _, _)],

    % === 非非 (最強の空間的跳躍 - これを最前列に持ってくるのが鍵) ===
    nextto(house(norwegian, _, _, _, _), house(_, _, _, _, blue), H),  % ← 最重要
    iright(house(_, _, _, _, ivory), house(_, _, _, _, green), H),     % ← 最重要

    % === 非 (早期否定・境界生成) ===
    member(house(_, _, _, coffee, green), H),   % greenが確定 → 残り空間が激減
    member(house(englishman, _, _, _, red), H),

    % === 亦 (構造的Debtの蓄積) ===
    member(house(spaniard, dog, _, _, _), H),
    member(house(ukrainian, _, _, tea, _), H),
    member(house(_, snails, winston, _, _), H),
    member(house(_, _, kools, _, yellow), H),
    member(house(_, _, luckystrike, oj, _), H),
    member(house(japanese, _, parliaments, _, _), H),

    % === 非非 (残りの跳躍) ===
    nextto(house(_, _, kools, _, _), house(_, horse, _, _, _), H),
    nextto(house(_, _, chesterfield, _, _), house(_, fox, _, _, _), H),

    % === 最終射影 (Dfix0指向) ===
    member(house(W, _, _, water, _), H),
    member(house(Z, zebra, _, _, _), H),

    % === 非中道の誤謬排除 (完全カット) ===
    !.

% 単一解版 + カット
zebra1(Houses, WaterDrinker, ZebraOwner) :-
    zebra(Houses, WaterDrinker, ZebraOwner), !.

% ベンチマーク（1000回実行）
benchmark1 :-
    flag(benchmark, _, 0),
    repeat,
    zebra1(_, _, _),
    flag(benchmark, N, N+1),
    N = 1000,
    !.

benchmark :- time(benchmark1).
