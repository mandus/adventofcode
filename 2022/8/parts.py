#!/usr/bin/env python3

from functools import reduce
from operator import mul

fn = 'test_input.txt'
fn = 'input.txt'

d = [[int(x) for x in li] for li in open(fn).read().split()]
wi = len(d)
he = len(d[0])


# helpers
def add(x, y):
    return (x[0]+y[0], x[1]+y[1])


def pos(m, x):
    return m[x[0]][x[1]]


def look_dir(m, x, d, w, h):
    def in_d(x):
        return 0 <= x[0] < w and 0 <= x[1] < h

    z = pos(m, x)
    cnt = 0
    while in_d(x := add(x, d)):
        cnt += 1
        if pos(m, x) >= z:
            break
    return cnt


visible = {}
# top/left
maxl = {}
maxt = {}
for i in range(wi):
    for j in range(he):
        h = d[i][j]
        if h > maxl.get(i, -1):
            visible[(i, j)] = True
            maxl[i] = h
        if h > maxt.get(j, -1):
            visible[(i, j)] = True
            maxt[j] = h

# bottom/right
maxr = {}
maxb = {}
for i in range(wi-1, -1, -1):
    for j in range(he-1, -1, -1):
        h = d[i][j]
        if h > maxr.get(i, -1):
            visible[(i, j)] = True
            maxr[i] = h
        if h > maxb.get(j, -1):
            visible[(i, j)] = True
            maxb[j] = h

print('part1 ', len(visible))


dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]
scenic = 0
for i in range(wi):
    for j in range(he):
        x = (i, j)
        scenic = max(scenic, reduce(
                    mul,
                    [look_dir(d, x, di, wi, he) for i, di in enumerate(dirs)]))
print('part2 ', scenic)
