#!/usr/bin/env k
d:0:"input.txt"
ds:" "\'d
elf:`A`B`C!0 1 2
me:`X`Y`Z!0 1 2
st:((4 8 3);(1 5 9);(7 2 6))
res:`X`Y`Z!(`A`B`C!2 0 1;`A`B`C!0 1 2;W:`A`B`C!1 2 0)

/funcs
map1:{elf[`$x[0]],me[`$x[1]]}
map2:{elf[`$x[0]],res[`$x[1]][`$x[0]]}
sc:{st[x[0];x[1]]}

`1:"part 1: "
+/sc'map1'ds

`1:"part 2: "
+/sc'map2'ds
