#!/usr/bin/env python3

import colorama as cl
import typing as ty
import heapq as hq


# fn = 't1.txt'
# fn = 't3.txt'
fn = 'input.txt'


def ins(sh, p):
    return 0 <= p[0] < sh[0] and 0 <= p[1] < sh[1]


def add(p: tuple, d: tuple) -> tuple:
    return p[0]+d[0], p[1]+d[1]


def g(d: list, p: tuple) -> int:
    r, c = p
    return d[r][c]


def re1(p: tuple, d: tuple, le: int, h: int) -> list:
    # where can we go
    r, c = d[0], d[1]
    g = [(p, (-c, -r), 0, h), (p, (c, r), 0, h)]
    if le < 3:
        g.append((p, d, le, h))
    return g


def re2(p: tuple, d: tuple, le: int, h: int) -> list:
    # we now go at least 4 but max 10 steps:
    r, c = d[0], d[1]
    if le < 4:
        return [(p, d, le, h)]
    g = [(p, (-c, -r), 0, h), (p, (c, r), 0, h)]
    if 4 <= le < 10:
        g.append((p, d, le, h))
    return g


def disp(sh: tuple, m: dict, curmin: int):
    dmap = {(0, 1): '\u2bc8',
            (0, -1): '\u2bc7',
            (1, 0): '\u2bc6',
            (-1, 0): '\u2bc5'}

    print(cl.Cursor.POS(3, 3) + f'current min: {curmin:4}')
    for r in range(sh[0]):
        for c in range(sh[1]):
            if (r, c) in m:
                print(cl.Cursor.POS(c+3, r+6) + cl.Fore.BLUE + dmap[m[(r, c)]], end='')
            else:
                print(cl.Cursor.POS(c+3, r+6) + cl.Fore.GREEN + '\u2588', end='')
    print()


def merge(s: dict, p: tuple, v: tuple, le: int, h: int) -> dict:
    pm = s.get(p, {})
    vm = pm.get(v, {})
    vm.update({le: h})
    pm[v] = vm
    s[p] = pm
    return s


def hl(d: list, sh: tuple, S: tuple, E: tuple, reach: ty.Callable) -> int:
    se = {}
    # points in queue: position, direction, num steps, current heat loss
    qu = [(S, (0, 1), 0, 0), (S, (1, 0), 0, 0)]
    hq.heapify(qu)

    mhl = sum(sum(x for x in r) for r in d)

    while qu:
        p, v, le, h = hq.heappop(qu)
        p, le = add(p, v), le+1

        if not ins(sh, p):
            # out of grid
            continue
        h = h + g(d, p)

        if h > mhl:
            continue

        if p == E:
            mhl = min(mhl, h)
            continue

        if p in se and v in se[p] and le in se[p][v] and max(hp for lp, hp in se[p][v].items() if lp <= le) <= h:
            # tested before with lower heat loss - move on
            continue
        se = merge(se, p, v, le, h)
        for cand in reach(p, v, le, h):
            hq.heappush(qu, cand)

    return mhl


def init():
    cl.just_fix_windows_console()
    print(cl.ansi.clear_screen() + cl.Cursor.POS(4, 2) + cl.Fore.BLUE + ' go... ')


def p1(d: list) -> int:
    sh = len(d), len(d[0])
    S = (0, 0)
    E = (sh[0]-1, sh[1]-1)
    return hl(d, sh, S, E, re1)


def p2(d: list) -> int:
    # init()
    sh = len(d), len(d[0])
    r, c = sh
    S = (0, 0)
    # need to have at least 4 straight at the end:
    E1 = (r-1, c-5)
    e1a = sum(d[r-1][i] for i in range(c-4, c))
    E2 = (r-5, c-1)
    e2a = sum(d[i][c-1] for i in range(r-4, r))
    return min(hl(d, sh, S, E1, re2) + e1a, hl(d, sh, S, E2, re2) + e2a)


d = [[int(c) for c in x] for x in open(fn).read().split()]

print(f'part1: {p1(d)}')
print(f'part2: {p2(d)}')
