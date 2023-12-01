#!/usr/bin/env k
/helpers
digp:{(x>"0"-1)&x<"9"+1} / is digit
chrp:{~digp x} / is not digit
/load
d:0:"input.txt"
/first and last digit:
fl:{.x[0],x[(#x)-1]}
`1:"part1: "
p1v:fl'(chrp_)'d / filter out chars; find first and last
+/p1v
`1:"part2: "
r:(("one";"o1e");("two";"t2o");("three";"t3e");("four";"f4r");("five";"f5e");("six";"s6x");("seven";"s7n");("eight";"e8t");("nine";"n9e")) / replacements
m:{[s]{y[1]/y[0]\x}/((,s),r)} / fold left; mutate orig string (here "s")
p2v:fl'(chrp_)'m'd / replace word with digits (m); filter out chars; find first and last
+/p2v
