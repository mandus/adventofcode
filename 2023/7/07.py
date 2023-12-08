#!/usr/bin/env python3

import functools as ft


fn = 't1.txt'
fn = 'input.txt'

d = [x.split() for x in open(fn).read().strip().split('\n')]


def grp(s: str, joker=False) -> dict:
    m = {}
    jcnt = 0
    for i in s:
        if joker and i == 'J':
            jcnt += 1
            continue
        m[i] = m.get(i, 0) + 1
    if joker:
        if jcnt == 5:
            m['J'] = 5
        else:
            m[sorted(m.items(), key=lambda x: x[1])[-1][0]] += jcnt
    return m


def hand(m: dict) -> int:
    ln = len(m)
    mv = sorted(m.values())
    if ln == 1:
        # 5 of a kind
        return 7
    if ln == 2:
        # 4 of a kind or full house
        return mv[-1] + 2
    if ln == 3:
        # 3 of a kind or two pairs
        return mv[-1] + 1
    if ln == 4:
        # one pair
        return 2
    return 1


def cmpfu(c, j=False):
    cards = str(c)
    joker = j

    def strth(x: str, y: str) -> int:
        xh, yh = hand(grp(x[0], joker)), hand(grp(y[0], joker))
        if xh != yh:
            return xh-yh
        else:
            for i, j in zip(x[0], y[0]):
                if i == j:
                    continue
                return cards.index(i) - cards.index(j)
    return strth


cmp = cmpfu("23456789TJQKA")
rnk = sorted(d, key=ft.cmp_to_key(cmp))
print(f'part1: {sum([(i+1)*int(j[1]) for i, j in enumerate(rnk)])}')


cmp = cmpfu("J23456789TQKA", True)
rnk2 = sorted(d, key=ft.cmp_to_key(cmp))
print(f'part2: {sum([(i+1)*int(j[1]) for i, j in enumerate(rnk2)])}')
