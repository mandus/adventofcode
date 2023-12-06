#!/usr/bin/env python3

import functools as ft

fn = 't1.txt'
fn = 'input.txt'

t, d = open(fn).read().strip().split('\n')

t2 = int(''.join(t.split(':')[1].split()))
d2 = int(''.join(d.split(':')[1].split()))

t = [int(x) for x in t.split(':')[1].split()]
d = [int(x) for x in d.split(':')[1].split()]


def dist(s, t):
    return s*t


wins = []
for g, b in zip(t, d):
    game = [dist(i, g-i) for i in range(g)]
    wins.append(len([g for g in game if g > b]))
print(f'part1: {ft.reduce(lambda x,y: x*y, wins)}')

print(t2, d2)
game = [dist(i, t2-i) for i in range(t2)]
print(f'part2: {len([g for g in game if g > d2])}')
