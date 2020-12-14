#!/usr/bin/env python3

import numpy as np
from functools import reduce

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
        done = len([v for v in val if v == m]) == len(fu)
        if not cnt % 100000:
            print(cnt, mul, val)
        cnt += 1
    return mul, m


def chinese_reminder(n, a):
    sum = 0
    prod = reduce(lambda a, b: a*b, n)
    for n_i, a_i in zip(n, a):
        p = prod // n_i
        sum += a_i * mul_inv(p, n_i) * p
    return sum % prod


def mul_inv(a, b):
    b0 = b
    x0, x1 = 0, 1
    if b == 1:
        return 1
    while a > 1:
        q = a // b
        a, b = b, a % b
        x0, x1 = x1 - q*x0, x0
    if x1 < 0:
        x1 += b0
    return x1


if __name__ == '__main__':
    print(routes)
    fus = buildfu(routes)
    print(find(fus))

    primes = [p[0] for p in routes]
    rems = [-p[1] for p in routes]
    print(chinese_reminder(primes, rems))
