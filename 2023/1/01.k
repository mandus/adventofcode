#!/usr/bin/env k

/helper
chrp:{x>"9"} / is char (in this context)
fl:{v:chrp_x;.(*v),*|v} /filter out chars; first and last digit:

/load
d:0:"input.txt"

`1:"part1: "
+/fl'd / find first and last digit in each str; reduce

/ alternate spelling; use str of digits; filterout non-nums in data and apply i
`1:"part1: "
i:""/$(1+!9) / str 1-9
+/{.(*x),*|x}'i@{x<0}_'i?d 

`1:"part2: "
r:(("one";"o1e");("two";"t2o");("three";"t3e");("four";"f4r");("five";"f5e");("six";"s6x");("seven";"s7n");("eight";"e8t");("nine";"n9e")) / replacements
m:{[s]{y[1]/y[0]\x}/((,s),r)} / fold left; mutate orig string (here "s")
+/fl'm'd / replace word with digits (m); find first and last digit in each str; reduce
