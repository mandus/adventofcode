#!/usr/bin/env k
d:0:"input.txt"
b:`red`green`blue!0 0 0
g:`red`green`blue!12 13 14
s:{dr:" "\'", "\x;(`$'*'|'dr)!.'*'dr}
u:{(!x)!x[!x]|y[!x]}
m:{u/b,(s'x)}'"; "\'*'": "\'*'|'": "\'d
`1:"part1: "; +/1+&{3=+/(.g-x[!g])>-1}'m
`1:"part2: "; +/{*/.x}'m
