#!/usr/bin/env python3

import numpy as np
import hashlib
import math

# input:
fullmoons = [np.array((17, 5, 1)), np.array((-2, -8, 8)), np.array((7, -6, 14)), np.array((1, -10, 4))]

# tests:
# moons = [np.array((-1, 0, 2)), np.array((2, -10, -7)), np.array((4, -8, 8)), np.array((3, 5, -1))]
# moons = [np.array((-8, -10, 0)), np.array((5, 5, 10)), np.array((2, -7, 3)), np.array((9, -8, -3))]


def tostr(x):
    return str(x)


def md5sum(moons, velo):
    s = "".join(["|".join(s for s in map(tostr, list(m))) for m in moons]) + \
        "".join(["|".join(s for s in map(tostr, list(v))) for v in velo])
    return hashlib.md5(s.encode('utf-8')).hexdigest()


def upd_m_v(moons, velo, n):
    for i in range(n):
        for j in range(n):
            if i == j:
                continue
            diff = moons[j]-moons[i]
            # abs_diff = np.array([v or 1. for v in np.abs(diff)])
            abs_diff = np.abs(diff) or 1.
            velo[i] += diff/abs_diff
    return [moons[i]+velo[i] for i in range(n)], velo


def run_component(moons):
    n = len(moons)
    velo = [np.zeros(1) for i in range(n)]
    moons, velo = upd_m_v(moons, velo, n)
    firsthash = md5sum(moons, velo)
    prevk = 0
    keeprunning = True
    k = 1
    while keeprunning:
        moons, velo = upd_m_v(moons, velo, n)
        hashit = md5sum(moons, velo)
        if hashit == firsthash:
            print("moon/velo (hash {}) also at k = {}; steps between: {}".format(
                hashit, prevk, k-prevk))
            keeprunning = False
            return(k-prevk)
        k += 1
        if not k % 10000:
            print(k)

#  part 1 - stop at k = 1000, and compute for all components at same time.
# print(velo)
# print(moons)
# pot = [np.sum(np.abs(m)) for m in moons]
# kin = [np.sum(np.abs(v)) for v in velo]
# tot = np.sum(np.array([pot[i]*kin[i] for i in range(n)]))
# print(pot)
# print(kin)
# print(tot)


c_p = []
for i in range(3):
    moons = [m[i] for m in fullmoons]
    c_p.append(run_component(moons))
left = int(c_p[0]*c_p[1]/math.gcd(c_p[0], c_p[1]))
right = int(c_p[1]*c_p[2]/math.gcd(c_p[1], c_p[2]))
lcm = int(left*right/math.gcd(left, right))

print("Part2: {}".format(lcm))
