#!/usr/bin/env python3

import colorama as cl


fn = 't1.txt'
fn = 'input.txt'


def ins(sh, p):
    return 0 <= p[0] < sh[0] and 0 <= p[1] < sh[1]


def add(p: tuple, d: tuple) -> tuple:
    return p[0]+d[0], p[1]+d[1]


def g(d: list, p: tuple) -> int:
    r, c = p
    return d[r][c]


def re(p: tuple, d: tuple, le: int, h: int) -> list:
    # where can we go
    r, c = d[0], d[1]
    g = [(p, (c, r), 0, h), (p, (-c, -r), 0, h)]
    if le < 3:
        g.append((p, d, le, h))
    return g


def disp(sh: tuple, m: dict, curmin: int):
    dmap = {(0, 1): '\u2bc8',
            (0, -1): '\u2bc7',
            (1, 0): '\u2bc6',
            (-1, 0): '\u2bc5'}

    print(cl.Cursor.POS(3, 3) + f'current min: {curmin}')
    for r in range(sh[0]):
        for c in range(sh[1]):
            if (r, c) in m:
                print(cl.Cursor.POS(c+3, r+6) + cl.Fore.BLUE + dmap[m[(r, c)]], end='')
            else:
                print(cl.Cursor.POS(c+3, r+6) + cl.Fore.GREEN + '\u2588', end='')


def merge(s: dict, p: tuple, v: tuple, le: int, h: int) -> dict:
    pm = s.get(p, {})
    vm = pm.get(v, {})
    vm.update({le: h})
    pm[v] = vm
    s[p] = pm
    return s


def max_if(li: list, m: int) -> bool:
    if li:
        return max(li)
    return m


def hl(d: list, sh: tuple, S: tuple, E: tuple) -> int:
    se = {}
    # hlm = {}
    # points in queue: position, direction, num steps, current heat loss
    qu = [(S, (0, 1), 0, 0), (S, (1, 0), 0, 0)]

    # mhl = sum(sum(x for x in r) for r in d)
    mhl = (sum(r[i+1] for i, r in enumerate(d) if i+1 < sh[0]) +
           sum(r[i] for i, r in enumerate(d)))
    # sum(r[i-1] for i, r in enumerate(d) if i > 0))

    while qu:
        p, v, le, h = qu.pop()
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

        # if (p, v, le) in se and se[(p, v, le)] < h:
        #     # tested before with lower heat loss - move on
        #     continue
        # se[(p, v, le)] = h
        if p in se and v in se[p] and max_if([hp for lp, hp in se[p][v].items() if lp <= le], mhl) <= h:
            # tested before with lower heat loss - move on
            continue
        se = merge(se, p, v, le, h)
        for cand in re(p, v, le, h):
            qu.insert(0, cand)

    return mhl


def init():
    cl.just_fix_windows_console()
    print(cl.ansi.clear_screen() + cl.Cursor.POS(4, 2) + cl.Fore.BLUE + ' go... ')


def p1(d: list) -> int:
    # init()
    sh = len(d), len(d[0])
    S = (0, 0)
    E = (sh[0]-1, sh[1]-1)
    return hl(d, sh, S, E)


d = [[int(c) for c in x] for x in open(fn).read().split()]

print(f'part1: {p1(d)}')
