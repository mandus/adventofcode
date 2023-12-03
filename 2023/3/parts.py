#!/usr/bin/env python3

import itertools as it

fn = 't1.txt'
fn = 'input.txt'


def isstar(c: chr) -> bool:
    return c == '*'


def issymb(c: chr) -> bool:
    return not c.isdigit() and not c == '.'


def adjc(i: int, j: int) -> set:
    return [(x, y) for x in range(i-1, i+2) for y in range(j-1, j+2)]


def group(t: list, i: int) -> list:
    if not t:
        return []

    def upd(ts):
        return int(''.join(x for x, _ in ts)), set([(i, y) for _, y in ts])

    g = []
    cur = []
    for n, j in t:
        if cur and j > cur[-1][1]+1:
            g += [upd(cur)]
            cur = []
        cur.append((n, j))
    return g + [upd(cur)]


def p_l(line: str, i: int) -> (list, list, list):
    nums = group([(c, j) for j, c in enumerate(line) if c.isdigit()], i)
    symbs = list(it.chain.from_iterable([adjc(i, j) for j, c in enumerate(line) if issymb(c)]))
    stars = [set(adjc(i, j)) for j, c in enumerate(line) if isstar(c)]
    return nums, symbs, stars


def parse(d: list) -> (list, list):
    numbers = []
    symbols = []
    stars = []
    for i, line in enumerate(d):
        n, s, st = p_l(line, i)
        numbers.extend(n)
        symbols.extend(s)
        stars.extend(st)
    return numbers, set(symbols), stars


d = open(fn).read().strip().split('\n')
nums, syms, stars = parse(d)
parts = [n for n, c in nums if c.intersection(syms)]
print(f'part1: {sum(parts)}')

gears = [gn[0]*gn[1] for gn in [[n for n, c in nums if c.intersection(c, st)] for st in stars] if len(gn) == 2]
print(f'part2: {sum(gears)}')
