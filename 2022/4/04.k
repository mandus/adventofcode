#!/usr/bin/env k
d:0:"input.txt"
ds:","\'d
m:{{.'x}'"-"\'x}'ds

`1:"part1: "
/ check if any is contained in the other;
:+/{{[a;b;c;d]((c>(a-1))&(d<(b+1)))|((a>(c-1))&(b<(d+1)))}[x[0][0];x[0][1];x[1][0];x[1][1]]}'m

r:{x+!(y+1-x)} / generate range
ovr:{~0=(#(1=)_.(#'=x))} / find intersection
`1:"part2: "
:+/{{[a;b;c;d]ovr(r[a;b],r[c;d])}[x[0][0];x[0][1];x[1][0];x[1][1]]}'m
