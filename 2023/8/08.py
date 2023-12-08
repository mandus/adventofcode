#!/usr/bin/env python3

fn = 't1.txt'
fn = 't2.txt'
fn = 'input.txt'

rl, d = open(fn).read().strip().split('\n\n')
m = {e[0]: {'L': e[2][1:-1], 'R': e[3][:-1]} for e in [x.split() for x in d.split('\n')]}

e = 'ZZZ'

pos = 'AAA'
cnt = 0
while pos != e:
    cnt += 1
    instr = rl[0]
    pos = m[pos][instr]
    rl = rl[1:] + instr

print(f'part1: {cnt}')
