#!/usr/bin/env python3

import numpy as np
import hashlib

# input:
moons = [np.array((17, 5, 1)), np.array((-2, -8, 8)), np.array((7, -6, 14)), np.array((1, -10, 4))]

# tests:
# moons = [np.array((-1, 0, 2)), np.array((2, -10, -7)), np.array((4, -8, 8)), np.array((3, 5, -1))]
# moons = [np.array((-8, -10, 0)), np.array((5, 5, 10)), np.array((2, -7, 3)), np.array((9, -8, -3))]

n = len(moons)
velo = [np.zeros(3) for i in range(n)]


def tostr(x):
    return str(x)


def md5sum(moons, velo):
    s = "".join(["|".join(s for s in map(tostr, list(m))) for m in moons]) + \
        "".join(["|".join(s for s in map(tostr, list(v))) for v in velo])
    return hashlib.md5(s.encode('utf-8')).hexdigest()


hashmap = {}

keeprunning = True
k = 0
while keeprunning:
    for i in range(n):
        for j in range(n):
            if i == j:
                continue
            diff = moons[j]-moons[i]
            abs_diff = np.array([v or 1. for v in np.abs(diff)])
            velo[i] += diff/abs_diff
    moons = [moons[i]+velo[i] for i in range(n)]
    hashit = md5sum(moons, velo)
    if not hashmap.get(hashit, False):
        # not present, insert
        hashmap[hashit] = k
    else:
        prevk = hashmap[hashit]
        print("moon/velo (hash {}) also at k = {}; steps between: {}".format(
            hashit, prevk, k-prevk))
        keeprunning = False
    k += 1
    if not k % 10000:
        print(k)

# part 1
print(velo)
print(moons)
pot = [np.sum(np.abs(m)) for m in moons]
kin = [np.sum(np.abs(v)) for v in velo]
tot = np.sum(np.array([pot[i]*kin[i] for i in range(n)]))
print(pot)
print(kin)
print(tot)
