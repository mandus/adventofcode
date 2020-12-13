#!/usr/bin/env python3

import numpy as np

fn = 'input_test.txt'
data = open(fn).read().strip().split('\n')
routes = [(int(x), i) for i, x in enumerate(data[1].split(',')) if x != 'x']


def findmul(d, r, t):
    n = 1
    v = (n*r) % d
    t = t % d
    while v != t:
        n += 1
        v = (n*r) % d
    return n


def makefu(rid, r0, wait):
    lcm = np.lcm(rid, r0)
    mul = findmul(r0, rid % r0, wait)*rid-wait

    def fu(n):
        return n*lcm + mul
    return fu


def buildfu(rs):
    r0 = rs[0][0]
    fu = []
    for r in rs[1:]:
        rid = r[0]
        wait = r[1]
        f = makefu(rid, r0, wait)
        fu.append(f)
    return fu


def find(fu):
    done = False
    mul = [0 for i in range(len(fu))]
    val = [-1 for i in range(len(fu))]
    m = 0
    cnt = 0
    while not done:
        for i in range(len(fu)):
            while val[i] < m:
                mul[i] = mul[i]+1
                val[i] = fu[i](mul[i])
        m = max(val)
        done = len([v for v in val if v == m]) == 4
        if not cnt % 100000:
            print(cnt, mul, val)
        cnt += 1
    return mul, m


fus = buildfu(routes)
print(find(fus))
