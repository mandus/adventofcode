#!/usr/bin/env python3

import functools as ft

fn = 't1.txt'
fn = 'input.txt'


def upd_pat(pat, qs, upd):
    for i, c in enumerate(upd):
        pat[qs[i]] = c
    return pat


def chk_pat(pat, chk):
    pat = list(reversed(pat))
    g = []
    in_group = False
    cnt = 0
    while pat:
        c = pat.pop()
        if c == '.':
            if in_group:
                g.append(cnt)
                gs = len(g)
                if g != chk[0:gs]:
                    return False
                cnt = 0
                in_group = False
        elif c == '#':
            cnt += 1
            if not in_group:
                in_group = True
    if in_group:
        g.append(cnt)
    return g == chk


def c_h(ln):
    return sum(1 for x in ln if x == '#')


@ft.lru_cache
def solve(pat, chk, cur=0):
    cnt = 0
    f = pat[0]
    r = pat[1:]

    if cur and (not chk or chk[0] < cur):
        return 0

    if not r:
        # exhausted;
        if f == '.' and cur and len(chk) == 1 and cur == chk[0]:
            return 1
        if f == '.' and not chk and not cur:
            return 1
        if f == '#' and len(chk) == 1 and chk[0] == cur+1:
            return 1
        if f == '?' and ((len(chk) == 1 and (chk[0] == cur or chk[0] == cur+1)) or (not chk and not cur)):
            return 1
        return 0

    if f == '.' and chk and chk[0] == cur:
        cnt += solve(r, chk[1:], 0)
    elif f == '.' and not cur:
        cnt += solve(r, chk, 0)
    elif f == '#':
        cnt += solve(r, chk, cur+1)
    elif f == '?' and chk and chk[0] == cur:
        # pretend dot and end group:
        cnt += solve(r, chk[1:], 0)
    elif f == '?':
        # pretend hash:
        tmp = solve(r, chk, cur+1)
        cnt += tmp
        # or pretend dot
        if cur and chk[0] == cur:
            cnt += solve(r, chk[1:], 0)
        elif not cur:
            cnt += solve(r, chk, 0)

    return cnt


def cnts(ln):
    pat, chk = ln.split()
    pat = tuple([c for c in pat])
    chk = tuple([int(x) for x in chk.split(',')])
    return solve(pat, chk)


def cnts2(ln):
    pat, chk = ln.split()
    pat = '?'.join([pat for _ in range(5)])
    chk = ','.join([chk for _ in range(5)])
    pat = tuple([c for c in pat])
    chk = tuple([int(x) for x in chk.split(',')])
    return solve(pat, chk)


d = open(fn).read().strip().split('\n')

print(f'part1: {sum([cnts(ln) for ln in d])}')
print(f'part2: {sum([cnts2(ln) for ln in d])}')
