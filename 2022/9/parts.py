#!/usr/bin/env python3

fn = 'test_input.txt'
fn = 'test2_input.txt'
fn = 'input.txt'

data = [(x.split()[0], int(x.split()[1]))
        for x in open(fn).read().strip().split('\n')]

dirs = {'R': (1, 0), 'U': (0, -1), 'L': (-1, 0), 'D': (0, 1)}


def move(x, d):
    return (x[0]+d[0], x[1]+d[1])


def dist(x, y):
    return max(abs(x[0]-y[0]), abs(x[1]-y[1]))


def movetail(h, t):
    if dist(h, t) > 1:
        x = t[0]
        y = t[1]
        if h[0] != t[0]:
            xdiff = h[0] - t[0]
            x = t[0] + xdiff//abs(xdiff)
        if h[1] != t[1]:
            ydiff = h[1] - t[1]
            y = t[1] + ydiff//abs(ydiff)
        return (x, y)
    return t


# part 1 - head and tail
h = (0, 0)
t = (0, 0)
seen = {t: True}
for i in data:
    (d, n) = i
    upd = dirs[d]
    for _ in range(n):
        h = move(h, upd)
        t = movetail(h, t)
        seen[t] = True

print('part1: ', len(seen))


# part 2 - 10 knots
ln = 10
kn = [(0, 0) for _ in range(ln)]
seen = {kn[-1]: True}
for i in data:
    (d, n) = i
    upd = dirs[d]
    for _ in range(n):
        kn[0] = move(kn[0], upd)
        for i in range(ln-1):
            kn[i+1] = movetail(kn[i], kn[i+1])
        seen[kn[-1]] = True

print('part2: ', len(seen))
