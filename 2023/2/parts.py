#!/usr/bin/env python3

import functools as ft

fn = 't1.txt'
fn = 'input.txt'
d = open(fn).read().strip().split('\n')
goal = {'red': 12, 'green': 13, 'blue': 14}


def upd(m, d):
    for dr in d:
        c, t = dr.split()
        m[t] = max(m.get(t, 0), int(c))
    return m


def game(g):
    i, d = g.split(': ')
    i = i[5:]
    m = {}
    for dr in d.split('; '):
        m = upd(m, dr.split(', '))

    pwr = ft.reduce(lambda x, y: x*y, [v for k, v in m.items()])

    if all([m.get(k, 0) <= v for k, v in goal.items()]):
        return int(i), pwr
    return 0, pwr


print(f'p1: {sum([game(g)[0] for g in d])}')
print(f'p2: {sum([game(g)[1] for g in d])}')
