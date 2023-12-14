#!/usr/bin/env python3

fn = 't1.txt'
# fn = 'input.txt'

d = [x.split('\n') for x in open(fn).read().strip().split('\n\n')]


def m(p):
    m = {}
    for j, ln in enumerate(p):
        indx = tuple(i for i, c in enumerate(ln) if c == '#')
        m[indx] = m.get(indx, []) + [j]
    return m


def mirrp(m):
    print([v for _, v in m.items() if len(v) >= 2])
    return len(m) - 1 <= len([k for k, v in m.items() if len(v) == 2])


def mirrs(m):
    print(sorted([v for _, v in m.items()]))


def refl(h):
    v = list(map(list, zip(*h)))
    hm = m(h)
    vm = m(v)
    print('h', hm, mirrp(hm))
    # if mirrp(hm):
    #     mirrs(hm)
    # else:
    #     mirrs(vm)
    print('v', vm, mirrp(vm))


print(f'part1: {[refl(p) for p in d]}')
