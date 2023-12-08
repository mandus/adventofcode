#!/usr/bin/env python3

import math

fn = 't3.txt'
fn = 'input.txt'

rl, d = open(fn).read().strip().split('\n\n')
m = {e[0]: {'L': e[2][1:-1], 'R': e[3][:-1]} for e in [x.split() for x in d.split('\n')]}


def steps(pos, mvs, m, p2=False):
    cnt = 0
    endchk = lambda x: x != 'ZZZ'
    if p2:
        endchk = lambda x: x[-1] != 'Z'
    while endchk(pos):
        cnt += 1
        instr = mvs[0]
        pos = m[pos][instr]
        mvs = mvs[1:] + instr
    return cnt


pos = 'AAA'
e = 'ZZZ'
if pos in m:
    print(f'part1: {steps(pos, rl, m)}')

cs = {p: 0 for p, _ in m.items() if p[-1] == 'A'}

for pos in cs.keys():
    cs[pos] = steps(pos, str(rl), m, True)
print(f'part2: {math.lcm(*list(cs.values()))}')
