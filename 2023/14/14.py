#!/usr/bin/env python3

fn = 't1.txt'
fn = 'input.txt'


def idx(li: list, c: chr) -> list:
    return [i for i, x in enumerate(li) if x == c]


def move_left(c: list) -> list:
    r = idx(c, 'O')
    s = idx(c, '#')
    sz = len(c)
    ret = []
    for i in s:
        t = [x for x in r if x < i]
        ret.extend(['O' for _ in t])
        cur = len(ret)
        while cur < i:
            ret.append('.')
            cur = len(ret)
        ret.append('#')
        for x in t:
            r.remove(x)
    ret.extend(['O' for _ in r])
    cur = len(ret)
    while cur < sz:
        ret.append('.')
        cur = len(ret)
    return ret


def rot(d: list) -> list:
    return list(map(list, zip(*d)))


def flip(d: list) -> list:
    return [list(reversed(x)) for x in d]


def move(d: list) -> list:
    return [move_left(c) for c in d]


def cycle(d: list) -> list:
    d = flip(move(flip(rot(flip(move(flip(rot(move(rot(move(rot(d))))))))))))
    return d


def fp(d: list) -> tuple:
    return tuple((i, j) for i, x in enumerate(d) for j, c in enumerate(x) if c == 'O')


def load(d: list) -> int:
    # make a copy
    d = list([list(x) for x in d])
    d = flip(rot(d))
    return sum([sum([i+1 for i, c in enumerate(x) if c == 'O']) for x in d])


def loop(d: list) -> tuple:
    d = list([list(x) for x in d])
    m = {fp(d): 0}
    it = 0
    while True:
        it += 1
        d = cycle(d)
        nxt = fp(d)
        if nxt in m:
            return it - m[nxt], m[nxt]
        m[nxt] = it


def count(d: list, cnt: int) -> list:
    d = list([list(x) for x in d])
    for _ in range(cnt):
        d = cycle(d)
    return d


d = [[c for c in ln] for ln in open(fn).read().strip().split('\n')]
print(f'part1: {load(rot(move(rot(d))))}')
lsz, init = loop(d)
modsz = (1000000000 - init) % lsz
print(f'part2: {load(count(d, init + modsz))}')
