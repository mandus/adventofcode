#!/usr/bin/env python3

import itertools as it

fn = 't1.txt'
# fn = 'input.txt'

d = [x.split('\n') for x in open(fn).read().strip().split('\n\n')]


def m(p):
    m = {}
    for j, ln in enumerate(p):
        indx = tuple(i for i, c in enumerate(ln) if c == '#')
        m[indx] = m.get(indx, []) + [j]
    return m


def g_p(ls: list) -> list:
    if len(ls) == 2:
        return [ls]
    return [[x, y] for i, x in enumerate(ls) for y in ls[1+i:]]


def mirrp(m, sz):
    # we don't need the patterns
    c = list(it.chain.from_iterable([g_p(v) for _, v in m.items() if len(v) >= 2]))
    st = [x for x in c if abs(x[0]-x[1]) == 1]
    # for each in start, see if we can reach either end
    for s in st:
        le, ri = s
        while [le, ri] in c:
            if le == 0 or ri == sz-1:
                return True, s
            le -= 1
            ri += 1
    return False, ()


def mirrs(m):
    print(sorted([v for _, v in m.items()]))


def refl(h):
    v = list(map(list, zip(*h)))
    hm = m(h)
    print(hm)
    vm = m(v)
    print(vm)
    tst, st = mirrp(hm, len(h))
    if tst:
        return 100*st[1]
    tst, st = mirrp(vm, len(v))
    if tst:
        return st[1]


print(f'part1: {sum(refl(p) for p in d)}')
