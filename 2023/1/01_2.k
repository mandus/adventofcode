#!/usr/bin/env k

d:0:"input.txt"

/ Heavily inspired from the solution in the ngn/k repo

/ simplest form of part 1 - but I don't understand really...
/ We are doing an ^/ which is null applied dyadically over 
/ I can use both ~ (not) and ^ (null) in the middle. 
/ With this we get a vector of ints instead of an array (of rank 1)
/ We do |: which take right and reverse; 1|: gives as a copy, and we then reverse again; |1|:
/ note: ^/' is reduce (foldL) on each, with ^ applied dyadically
/ dyadic ^ is "fill" - if one value is null it is replaced with the other (non-null)
/ When applied dyadically over a list, we effectively get the last non-null value
/ Finally, decode into base-10 to make it into digits of a number

/+/{10/^/'|1|:\~'/y?,$x}\:[d;$!10]
/+/{10/^/'|1|:\^'/y?,$x}\:[d;$!10]

i:$!10
`1:"p1: "
+/{10/^/'|1|:\^/i?,$x}'d

/ assume y is an int; y':x is then windows over x. 
/ We apply on length of x to always get same number of parts (else we get problem with ^'/)
g:{(y':x)@!#x}

/ we use each-right on the inner func g; the whole left with each of the right
/ and the right is the distinct (?) lengths (#) over i  
/ again we use i? to find the matches in i (in the windows) - note that we have arranged i such that `two is in position 12, etc
/ rest follows as above; here we need \^' since we have one vector for each window size
/ and we need 10! to mod (  12 => 2 for `two; etc)
`1:"p2: "
i:$(!10),0,`one`two`three`four`five`six`seven`eight`nine
+/{10/10!^/'|1|:\^'/i?g/:[x;?#'i]}'d
