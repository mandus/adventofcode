#!/usr/bin/env python3

import itertools as it

# fn = 't1.txt'
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

def arrs(ln):
    pat, chk = ln.split()
    pat = [c for c in pat]
    chk = [int(x) for x in chk.split(',')]
    t_h = sum(chk)
    h = c_h(pat)
    # need to have this amount of '#' in the updates
    f_h = t_h - h

    # need to find all the permutations of # for the question-marks in pat
    # that match chk and the rule (at least on . between groups)
    qs = [i for i, q in enumerate(pat) if q == '?']
    upds = ("".join(seq) for seq in it.product(".#", repeat=len(qs)) if c_h(seq) == f_h)
    return sum(1 for u in upds if chk_pat(upd_pat(pat, qs, u), chk))


d = open(fn).read().strip().split('\n')

print(f'part1: {sum([arrs(ln) for ln in d])}')
print(f'part2: {2}')


# ..#.??.#; 1,1,1
# ..#.#..#
# ..#..#.#
