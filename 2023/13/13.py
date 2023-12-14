#!/usr/bin/env python3

import itertools as it

fn = 't1.txt'
fn = 'input.txt'

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


def upd_m(m, v, w):
    m_c = dict(m)
    lv, lw = len(m_c[v]), len(m_c[w])

    # TODO: If neither is 1, I need to loop over and try
    # smudging one by one and see what works?
    #
    if not (lv == 1 or lw == 1):
        return m, False

    if lv > lw:
        m_c[v] = sorted(list(m_c[v]) + m_c[w])
        del m_c[w]
    else:
        m_c[w] = sorted(list(m_c[w]) + m_c[v])
        del m_c[v]
    return m_c, True


def smudge(m, ln):
    sgs = [k for k, v in m.items()]
    for i, sv in enumerate(sgs):
        lv = len(sv)
        for sw in sgs[1+i:]:
            lw = len(sw)
            if abs(lv - lw) != 1:
                continue

            if lv > lw:
                fix = tuple(i for i in sv if i not in sw)
                if len(fix) == 1:
                    m_c, chg = upd_m(m, sv, sw)
                    if not chg:
                        continue
                    tst, st = mirrp(m_c, ln)
                    if tst:
                        return m_c, True
            else:
                fix = tuple(i for i in sw if i not in sv)
                if len(fix) == 1:
                    m_c, chg = upd_m(m, sv, sw)
                    if not chg:
                        continue
                    tst, st = mirrp(m_c, ln)
                    if tst:
                        return m_c, True
    return m, False


def refl(h, c_s=False):
    # c_s: correct smudge

    v = list(map(list, zip(*h)))

    hm = m(h)
    vm = m(v)

    if c_s:
        hm, chgd = smudge(hm, len(h))
        if not chgd:
            vm, chgd = smudge(vm, len(v))

    tst, st = mirrp(hm, len(h))
    if tst:
        return 100*st[1]
    tst, st = mirrp(vm, len(v))
    if tst:
        return st[1]


print(f'part1: {sum(refl(p) for p in d)}')
print(f'part2: {sum(refl(p, True) for p in d)}')
