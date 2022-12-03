#!/usr/bin/env k
d:0:"input.txt"
/ split in halfs for part 1
sp:{2 0N#x}'d
/ group in 3 for part 2
g:0N 3#d


/ How can I make the 2/3 a param?
dupe:{(#'=(,/?'x))?2}
tri:{(#'=(,/?'x))?3}

/ mn:26 + `i$"a" - `i$"A" / modolus number; There are something between "Z" and "a"!
/ res:{+/mn!26+x-(`i$("A")-1)}
/ actually, the above can be simplified as 26 + "a" - "A" = 58
res:{+/58!x-96}

`1:"part 1: "
res (dupe'sp)

`1:"part 2: "
res (tri'g)
