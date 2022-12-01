#!/usr/bin/env k
d:0:"input.txt"
e:&~#'d / find the length of each field- some will be zero. Use not (~) to find the zeros (now 1) and use  where (&) to find these indecis
n:1_'e_d / use the list e to cut (_) d at the empty fields (now first in each entry) and use 1_ over (') to drop 1 from each list, leaving only the numbers
l:(+/.')'n / use . (eval) over (') each of the lists and then reduce with sum (+/), do this over each sublist in n
l[*>l]  / sort index of l descending, take first, and look up in l
+/l[3#>l] / similarly, take the 3 first and sum them

/ we don't need e and n, that was just to make it more readable:
l:(+/.')'(1_'(&~#'d)_d)
l[*>l]
+/l[3#>l]
