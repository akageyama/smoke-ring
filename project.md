---
project: Smoke Ring
summary: 渦輪の形成と伝播の3次元シミュレーション + 可視化
src_dir: ./src
output_dir: ./doc
media_dir: ./media
exclude_dir: ./src/tests
             ./src/tests/introspection
predocmark_alt: >
predocmark: <
display: public
         protected
         private
source: true
graph: true
sort: alpha
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
---

--------------------


## 概要

  3次元ナビエ・ストークス方程式を解いて渦輪の形成と伝播のシミュレーションを行う。

## シミュレーション領域

  直方体領域。3次元周期境界条件。カーテシアン座標。

## 計算手法

  空間離散化は2次中心差分法。時間積分は4次ルンゲ・クッタ法。

## 実行方法

  * cd src
  * make
  * cd ../job
  * qsub sample.js
 


