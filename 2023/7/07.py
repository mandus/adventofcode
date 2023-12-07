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
    if ln == 1:
        # 5 of a kind
        return 7
    if ln == 2:
        # 4 of a kind or full house
        return 6 if sorted(m.values())[0] == 1 else 5
    if ln == 3:
        # 3 of a kind or two pairs
        return 4 if sorted(m.values())[-1] == 3 else 3
    if ln == 4:
        # one pair
        return 2
    return 1


def strth(x: str, y: str) -> int:
    cards = "23456789TJQKA"
    xh, yh = hand(grp(x[0])), hand(grp(y[0]))
    if xh != yh:
        return xh-yh
    else:
        for i, j in zip(x[0], y[0]):
            if i == j:
                continue
            return cards.index(i) - cards.index(j)


def strth2(x: str, y: str) -> int:
    cards = "J23456789TQKA"
    xh, yh = hand(grp(x[0], True)), hand(grp(y[0], True))
    if xh != yh:
        return xh-yh
    else:
        for i, j in zip(x[0], y[0]):
            if i == j:
                continue
            return cards.index(i) - cards.index(j)


rnk = sorted(d, key=ft.cmp_to_key(strth))
print(f'part1: {sum([(i+1)*int(j[1]) for i, j in enumerate(rnk)])}')


rnk2 = sorted(d, key=ft.cmp_to_key(strth2))
print(f'part2: {sum([(i+1)*int(j[1]) for i, j in enumerate(rnk2)])}')
