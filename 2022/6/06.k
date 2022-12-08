#!/usr/bin/env k
d:*0:"input.txt"
w:{x':d}
sol:{x+(#'?'(w x))?x} /using 'uniq' (?) over each window, and 'rank' of each (#'); find the first one that is full length. 
sol:{x+*>x(x=#?:)':d} /alternate spelling; use window with test inserted; x(x=#?:)'
`1:"part1 "
sol 4
`1:"part2 "
sol 14
