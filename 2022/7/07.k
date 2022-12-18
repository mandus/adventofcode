#!/usr/bin/env k
/:d:-1_'1_'1_"$"\1:"test_input.txt"
/pwd:,""
/cd:{"dir ",3_x}
/ls:{"ls ",x}
/{$["cd"~2#x;cd x;"ls"~2#x;ls x]}'d

/x:0:"test_input.txt"
x:0:"input.txt"
t:()!0 /total sizes
f:{$["$ cd .."~x;d::-1_d
   "$ cd /"~x;d::,""
   "$ cd "~5#x;d,:*|" "\x
   "$ ls"~x; / do nothing
   "dir "~4#x; / do nothing
   ~"0:"'*x; t["/"/',\,'d]+:`I$*:" "\x
   unknown]}
f'x;
+/t@&t<100000         /part1
&/t@&~t<-40000000+t"" /part2
