#!/usr/bin/env k
/ This is from @yernab on youtube in his experimenting with ngn/k video in October 2023.
/ set p as data, and r as the rules (second part of input)

r:" -> "\'2_p:0:"input.txt"
/ could join this in front of the line above if we want, but let's keep it a bit simple
/ cast the first part (*) (the keys) of r into symbols (`$)
i:`$*'r
/ build function
/ input (x) to this function will be list of symbols, which we do a window function over ($2'). 
/ Further, we group (=`) to find indecis in (something??) and finally we take the count (#')
/ This we apply on i (index into i) to get the count of each symbol, and we fill with zeros (0^) to avoid false
/ note; this function use i internally.
b:{0^(#'=`$2':x)@i}
/ starting vector
s:b@*p
/ each row in r, splice the y into x (split x and join (/) with y) to get the triplets. Run b on these 
/ which gives us (where in i) the two pairs are
/ note: this is for r which contains all rules - so the matrix gives as _all_ the transitions
m:{b@y/x}.'r
/ now we apply m on the start vector, and sum up each row:
// +/m*s
/ but actually, we can use a "train" and apply this over and over again, e.g
// 2(+/m*)/s
// 10(+/m*)/s
/ we now like to find the indecis for each char in the list of rules - so we join the i, the keys with ,/$
/ and then we group - but we need to divide by two to find the correct index as we expand each pair into two chars:
c:-2!=,/$i
/ alternatively we can use floor (_) and divide (%) - but then its a new func in play:
/ - c can be assigned to this as well - more keystrokes, but easier to read:
// :c:{_x%2}'=,/$i
/ diff function - subtract smallest (&/) from largest (|/) in arg:
d:{|/x-&/x}
/ let's put it together (note the '10' now goes on the end because it's a func - so the 'x' has the role now for repeating the train
/ - index into (@) c (the characters)
/ - sum for each char - but we will add each char twice as each will be both in left and right par (last and first char respectively)
/ so we again need to divide by two, except that the first and last char will not be doubled as they will be
/ just first of first pair and last of last pair
/ as we don't have a ceil, we flip sign (-), then integer-divide by 2 again (-2!), and flip sign again (-)
/ finally, apply diff (d) on the list to get our answer
f:{d@--2!-+/'(x(+/m*)/s)@c}
f'10 40
