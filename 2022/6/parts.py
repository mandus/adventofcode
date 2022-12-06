#!/usr/bin/env python3

fn = 'input.txt'

d = open(fn).read().strip()

def find_start(instr, win=4):
    pos = 0
    s = set(instr[pos:pos+win])
    while len(s) < win:
        pos += 1
        s = set(instr[pos:pos+win])
    return pos + win


print(f'part1: {find_start(d, 4)}')
print(f'part2: {find_start(d, 14)}')
