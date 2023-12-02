#!/usr/bin/env k
d:0:"input.txt"
g:`red`green`blue!12 13 14
s:{dr:" "\'", "\x;(`$'*'|'dr)!.'*'dr}
m:{|/s'x}'"; "\'*'": "\'*'|'": "\'d
`1:"part1: "; +/1+&{3=+/(.g-x[!g])>-1}'m
`1:"part2: "; +/*/'m
