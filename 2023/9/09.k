#!/usr/bin/env k

d:.'0:"input.txt"
f:{+/2_|*'|'{1_-':x}\x}

`1:"part1: "
+/f'd

`1:"part2: "
+/f'|'d
