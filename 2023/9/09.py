#!/usr/bin/env python3

fn = 't1.txt'
fn = 'input.txt'

d = [[int(y) for y in x.split()] for x in open(fn).read().strip().split('\n')]


def diff(ln):
    return [y-x for x, y in zip(ln[:-1], ln[1:])]


def scan(ln):
    lns = [ln]
    while any(lns[-1]):
        lns.append(diff(lns[-1]))
    return sum([ln[-1] for ln in lns])


print(f'part1: {sum([scan(x) for x in d])}')
print(f'part2: {sum([scan(list(reversed(x))) for x in d])}')
