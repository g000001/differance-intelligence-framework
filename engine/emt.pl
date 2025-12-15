% -*- mode: prolog -*-
% ============================================================
%  EMT (Emergent Middle Test) — 空・乱起・縁起・中道の4層モデル
%  Improved CLPFD Reference Prototype
% ============================================================

:- use_module(library(clpfd)).
:- use_module(library(lists)).

% ------------------------------------------------------------
% 1. 「空 (emptiness)」— 未確定な初期状態と探索空間
% ------------------------------------------------------------

% 空は「未確定 → 乱起が生まれる余地」を表すため、数値0ではなく
% CLP 変数として扱う（＝本来の空の意味に近くなる）。
emptiness_state(llm, E_State, 1..10000) :-
    E_State in 1..10000.

emptiness_state(ga, E_State, 1..1000) :-
    E_State in 1..1000.


% ------------------------------------------------------------
% 2. 「乱起 (ranki)」— 空から生じる非決定的ゆらぎ生成
% ------------------------------------------------------------

% 「乱起」は Prolog の非決定性 (indomain/1) を使って表現。
% E_State の領域に従い、複数の提案（semantic candidates）を生成する。
ranki_proposals(E_State, Proposals) :-
    length(Proposals, 10),  % 提案数をとりあえず10個に
    maplist(generate_ranki(E_State), Proposals).

generate_ranki(E_State, X) :-
    E_State in 1.._,
    X in 1..100,
    indomain(X).  % 非決定性＝乱起


% ------------------------------------------------------------
% 3. 「縁起 (dependent origination)」— DIFW 的エネルギー評価
% ------------------------------------------------------------

% DIFW の Godel-L1 にある3つの構造からなる「意味エネルギー」
% SC = semantic coherence
% CL = chain length penalty
% OD = oscillation degree
% E_Total = SC + CL + OD

evaluate_proposal(X, E_Total) :-
    semantic_coherence(X, SC),
    chain_penalty(X,      CL),
    oscillation_degree(X, OD),
    E_Total #= SC + CL + OD.

% --- 以下は簡易モデル（本実装ではドメイン固有に書き換え推奨） ---

semantic_coherence(X, SC) :-
    SC #= abs(X - 50).     % 例: 50付近が意味的安定点

chain_penalty(X, CL) :-
    CL #= (X mod 7).       % 例: 適当に著者補正できる部分

oscillation_degree(X, OD) :-
    OD #= (100 - X) // 10. % 例: X が大きいほど安定


% ------------------------------------------------------------
% 4. 「中道 (Middle Way)」— 最小エネルギーへの収束
% ------------------------------------------------------------

dependent_origination(Proposals, MinE, MiddleWay) :-
    % 1. すべての提案について E を計算
    maplist(eval_pair, Proposals, Pairs),
    % Pairs = [X-E, X-E, ...]
    
    % 2. 最小エネルギー値を求める
    min_member(_-MinE, Pairs),
    
    % 3. その MinE を持つ X を MiddleWay とする
    member(MiddleWay-MinE, Pairs),
    non_oscillatory(MiddleWay).

eval_pair(X, X-E) :-
    evaluate_proposal(X, E).

non_oscillatory(M) :-
    % 中道は“揺れない”という DIFW/EMT 的条件（軽い版）
    M #>= 1,
    M #=< 100.


% ------------------------------------------------------------
% 5. 最終判定 — EMT による「知性の創発」判定
% ------------------------------------------------------------

is_intelligent(System, MiddleWay) :-
    % 1. 空（初期状態）
    emptiness_state(System, E_State, _Domain),

    % 2. 乱起（ゆらぎ生成）
    ranki_proposals(E_State, Proposals),

    % 3. 縁起（制約充足）＋ 中道（最小矛盾）
    dependent_origination(Proposals, MinE, MiddleWay),

    % 4. 中道（Middle Way）が実在しているか？
    MinE #< 100,
    MiddleWay \== [],
    
    format("~w passed EMT. Middle Way = ~w (Energy ~w)~n",
           [System, MiddleWay, MinE]).
