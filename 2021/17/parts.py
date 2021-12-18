#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'


def trnsf(x):
    return list(map(int, x[2:].strip(',').split('..')))


def stepx(x, t):
    t = t + 1
    f = max(0, x-t+1)
    return sum(range(f, x+1)), t


def stepy(y, t):
    t = t + 1
    to = y - t
    return sum(range(y, to, -1)), t


def xcand(x):
    xl, xr = x
    cx = 1
    mapx = {}
    while cx <= xr:
        t = 0
        ptx = 0
        tx = -1
        while ptx != tx:
            ptx = tx
            tx, t = stepx(cx, t)
            if xl <= tx <= xr:
                mapx[cx] = mapx.get(cx, []) + [(tx, t)]
            if tx > xr:
                break   # overshoot - move to next x
        cx += 1
    return mapx


def ycand(y, high):
    yb, yt = y
    cy = yb
    mapy = {}
    while cy <= high:
        t = 0
        ty = yt + 1
        while ty >= yb:
            ty, t = stepy(cy, t)
            if yb <= ty <= yt:
                mapy[cy] = mapy.get(cy, []) + [(ty, t)]
        cy += 1
    return mapy


def tlisthas(lst, ts):
    return ts in [e[1] for e in lst]


def tlisthasgeq(lst, ts):
    return any([e[1] for e in lst if e[1] >= ts])


def ybyt(yopts, ts):
    return [y for y, tclist in yopts.items() if tlisthas(tclist, ts)]


def ybygeqts(yopts, ts):
    return [y for y, tclist in yopts.items() if tlisthasgeq(tclist, ts)]


def vertical(xcands):
    # Find those x candidates that will end inside target - i.e two last have same target-value
    opts = []
    for x, topts in xcands.items():
        if len(topts) >= 2 and topts[-2][0] == topts[-1][0]:
            opts.append((x, topts[-2][1]))
    return opts


data = list(map(trnsf, open(fn).readline().strip().split()[2:]))
xopts = xcand(data[0])
vx = vertical(xopts)

high = abs(min(data[1]))
if min(x[0] for x in vx) < high:
    print('part1 ', sum(range(high)))

yopts = ycand(data[1], high)

hits = {}
for x, tslist in xopts.items():
    for ts in tslist:
        match = ybyt(yopts, ts[1])
        for y in match:
            hits[(x, y)] = 1

# extra hits for verticals
for x, ts in vx:
    for y in ybygeqts(yopts, ts):
        hits[(x, y)] = 1

print('part2', len(hits))
