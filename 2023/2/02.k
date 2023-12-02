#!/usr/bin/env k
d:0:"input.txt"

b:`red`green`blue!0 0 0 /set init state
g:`red`green`blue!12 13 14 /goal

s:{dr:" "\'", "\x;(`$'*'|'dr)!.'*'dr} /split game into grabs
u:{(!x)!x[!x]|y[!x]} /update map
c:{3=+/(.g-x[!g])>-1}  /check if game max lower than equal to goal

gs:"; "\'*'": "\'*'|'": "\'d /games
m:{u/b,(s'x)}'gs /max; fold updater over base and grabs for each game

`1:"part1: "
+/1+&c'm / check goal for all max. Need 1+ since we're zero-indexed

`1:"part2: "
+/{*/.x}'m /just multiply each max and add up
