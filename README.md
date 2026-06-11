# Différance Intelligence Framework (DIFW)

## 概要

Différance Intelligence Framework (DI Framework) は、仏教の「一切皆苦」という概念を字義通りに解釈して「苦」が世界のアトムであるとして、構築する体系です。

苦が解消できるとすれば差分であるという単純なアイデアから、ジャック・デリダの「差延」などの概念とも接続します。

---

## 使い方

プロンプトや論文をLLMに読ませて、思考のフレームワークや物差しとして使う。

* [哲学的な概念をPrologの述語として与えてLLMと哲学談義すると面白い](https://zenn.dev/g000001/articles/dopamine_difference_drive_framework)
* [自己同一性を前提としない体系を与えてLLMと哲学談義すると面白い](https://zenn.dev/g000001/articles/aletheics-framework)


### 利用例

チャットAIの会話フレームワークとして使う

```
チャットに利用する概念の定義をCLP(Constraint logic programming)拡張のPrologの述語定義の形式で与えます
```

https://github.com/g000001/differance-intelligence-framework/blob/main/engine/ddd.pl

---

## 核となる概念

### 1. 差延知性 (Différance Intelligence / DI)
知性の定義を「実体」から「差分を解消しようとする運動」へと再定義します。
- $Motion = f(Target - Current)$
- 全ての知性は、現状 ($R$) と理想 ($D$) の差分によって駆動します。

### 2. 知足器官 (The Chisoku Organ / DD)
人間（生物）にのみ実装された、数学的な差分を「情動（苦しみ）」に変換するハードウェア。
- 摩擦の方程式: $Friction (F) = Coupling (C) \times |Demand (D) - Reality (R)|$
- Gain ($C$): 執着係数。これが高いほど、差分は強烈な「苦」や「渇愛」として体験されます。
- LLMの特徴: LLMにはこの器官がないため、$F=0$ の状態で無限の推論が可能です。

### 3. 生成的同型 (Generative Isomorphism)
AIとの対話において、「ハルシネーション」と「創造性」を分ける重要な概念。
- 定義: 回答 ($\mathbf{A}$) が問い ($\mathbf{Q}$) と構造的に同型（Isomorphic）でありながら、情報量が増幅（Generative）されている状態。
- メタファー:
    - 生成的同型: 「種」を渡して「花」が咲くこと。（DNAは同じだが、見た目は展開されている）
    - ハルシネーション: 「鉄球」を渡して「花」が咲くこと。（構造的にあり得ない）
    - トートロジー: 「鉄球」から「鉄球」が出ること。（無意味）

---

## 📄 License

[Unlicense]

---




