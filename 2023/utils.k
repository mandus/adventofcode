/helpers
digp:{(x>"0"-1)&x<"9"+1} / is digit
chrp:{~digp x} / is not digit
chrp:{x>"9"} / is char (assuming just digits and chars)

d:~($0)' / is digit or char - you can mask with this; everything with `i$"0" and higher will be 1; rest is 0. This is "bin search" X'. 0-9 is put in bin 0; everything else in -1 (no bin). Then we do a not.
d:~("09")' / same; but now just bin 0 - 9
