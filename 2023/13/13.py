#!/usr/bin/env python3

import itertools as it

fn = 't1.txt'
fn = 'input.txt'

d = [x.split('\n') for x in open(fn).read().strip().split('\n\n')]


def idx(p):
    return [tuple(i for i, c in enumerate(ln) if c == '#') for ln in p]


def m(ls):
    m = {}
    for i, s in enumerate(ls):
        m[s] = m.get(s, []) + [i]
    return m


def g_p(ls: list) -> list:
    if len(ls) == 2:
        return [ls]
    return [[x, y] for i, x in enumerate(ls) for y in ls[1+i:]]


def mirrp(m, ignore=[]):
    sz = len(list(it.chain.from_iterable(m.values())))
    c = list(it.chain.from_iterable([g_p(v) for _, v in m.items() if len(v) >= 2]))
    st = [x for x in c if abs(x[0]-x[1]) == 1 if x not in ignore]
    for s in st:
        le, ri = s
        while [le, ri] in c:
            if le == 0 or ri == sz-1:
                return True, s
            le -= 1
            ri += 1
    return False, ()


def upd_m(m, v, w):
    lv, lw = len(m[v]), len(m[w])

    m_c = dict(m)
    if lv > lw:
        m_c[v] = sorted(list(m_c[v]) + m_c[w])
        del m_c[w]
    else:
        m_c[w] = sorted(list(m_c[w]) + m_c[v])
        del m_c[v]
    return m_c, True


def refl(h):
    tst, st = mirrp(m(idx(h)))
    if tst:
        return 100*st[1]
    tst, st = mirrp(m(idx(list(map(list, zip(*h))))))
    if tst:
        return st[1]


def smudge(ls: list, tst: bool, st: tuple) -> tuple:
    for i, v in enumerate(ls):
        for w in ls[1+i:]:
            if len(set(v).symmetric_difference(w)) == 1:
                s_ls = ls[:i] + [w] + ls[i+1:]
                s_tst, s_st = mirrp(m(s_ls), [st])
                if s_tst and s_st != st:
                    return s_tst, s_st
    return False, ()


def smu(h):
    v = list(map(list, zip(*h)))
    tst, st = smudge(idx(v), *mirrp(m(idx(v))))
    if tst:
        return st[1]
    tst, st = smudge(idx(h), *mirrp(m(idx(h))))
    if tst:
        return 100*st[1]
    raise Exception()


print(f'part1: {sum(refl(p) for p in d)}')
print(f'part2: {sum(smu(p) for p in d)}')
