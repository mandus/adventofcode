#!/usr/bin/env python3

fn = 't1.txt'
fn = 'input.txt'
x = [[c for c in ln] for ln in open(fn). read().strip().split('\n')]


def addempty(d: list) -> list:
    m = len(d[0])
    ex = 0
    y = list(d)
    for i, ln in enumerate(d):
        if '#' not in ln:
            ex += 1
            y = y[0:i+ex] + [['.' for _ in range(m)]] + y[i+ex:]
    return y


def extras(d: list) -> list:
    return [i for i, ln in enumerate(d) if '#' not in ln]


def transp(d: list) -> list:
    return list(map(list, zip(*d)))


def calcdists(x: list, exr: list, exc: list, mul: int = 2) -> list:
    g = [(i, j) for i, ln in enumerate(x) for (j, c) in enumerate(ln) if c == '#']
    dists = []
    extra = []
    for i, v in enumerate(g):
        xv, yv = v
        for w in g[i+1:]:
            xw, yw = w

            xl, xh = sorted([xv, xw])
            yl, yh = sorted([yv, yw])

            addx = len([i for i in exc if xl < i < xh])
            addy = len([i for i in exr if yl < i < yh])

            d = abs(xv-xw) + abs(yv-yw)
            extra.append(addx + addy)
            dists.append(d)
    dists.append(sum(extra)*(mul-1))
    return dists


# find the empty rows and columns:
xcols = extras(transp(x))
xrows = extras(x)

print(f'part1: {sum(calcdists(x, xcols, xrows))}')
print(f'part2: {sum(calcdists(x, xcols, xrows, mul=1000000))}')
