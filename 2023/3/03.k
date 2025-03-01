#!/usr/bin/env k

/ heavily inspired by solution in ngn/k repo

d:0:"input.txt"
i:~"0:"'
n:i d          / numbers (: is next to 9 in ordinal value)
s:~n|"."=d     / symbols (not numbers or .)
m*:+\0>':m:,/n / enumerate the number-positions
p:.(,0)_=m     / global positions of digits
v:`I$(,/d)p    / values
A:{3|/':0,x,0} / use stencil with 3, and max over to put 1 around other 1's
a:A'A s        / put a 1 around all symbols
O:(?m*,/a)^0   / multiply m with a to find where digits and symbol-neigbours overlap. Filter out zero
`1:"p1: "
+/v@-1+O
+/v@-1+(?m*,/A'A s)^0

S:#'1*:\d
D:1-!3 3
G:&"*"=d
h:+m@S/(S-1)&0|D+\:'G
`1:"p2: "
+/{*/v[r-1]*2=#r:(?x)^0}'h
