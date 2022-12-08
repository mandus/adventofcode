#!/usr/bin/env k
d:0:"input.txt"
e:d?""
c:e#d
m:" "\'1_e_d
l:#.c[-1+#c]
c:(^:)_'(+c)1+4*!l
(m;f;t):0 -1 -1+`I$(+m)1 3 5

mv:{[n;f;t](n_f;(n#f),t)} /move num boxes from f to t
sol:`0:*'c{[c;m;f;t]@[c;f,t;:;mv[m;c f;c t]]}/ / use amend over the crates
`1:"part1 "
sol[1;{m}#f;{m}#t]

`1:"part2 "
sol[m;f;t]
