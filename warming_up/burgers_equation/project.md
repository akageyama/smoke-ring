---
project: Burgers Equation
summary: 1次元バーガース方程式のシミュレーション + 可視化
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


### 概要

  これは、神戸大学情報知能工学科の講義 "HPC" （B3対象）用サンプルコードである。
  1次元バーガース方程式を差分法とルンゲ・クッタ積分法で解き、可視化する。
 
### 目的
  この後に説明する3次元Smoke-Ringシミュレーションコードを理解するため。

  3次元Smoke-Ringシミュレーションコードでは
  基本方程式は違うものの、アルゴリズム（差分法+ルンゲ・クッタ積分法）
  とシミュレーションコードの構造が同じである。
 
### 実行方法
>  cd src ; make      
     


