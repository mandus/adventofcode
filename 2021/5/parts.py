#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'


def splitints(x):
    return [int(i) for i in x.split(',')]


def incr(a, b):
    if a == b:
        return 0
    return int((b - a)/abs(b - a))


data = [[splitints(x) for x in line.strip().split()[::2]] for line in open(fn, 'r')]

def coords(x, y, diag=False):
    x1, y1 = x[:]
    x2, y2 = y[:]
    if not (x1 == x2 or y1 == y2) and not diag:
        return []
    xd = incr(x1, x2)
    yd = incr(y1, y2)
    res = [(x1, y1)]
    while (x1 != x2) or (y1 != y2):
        x1 += xd
        y1 += yd
        res.append((x1, y1))
    return res


cmap_1 = {}
cmap_2 = {}

for d in data:
    for c in coords(*d):
        cmap_1[c] = cmap_1.get(c, 0) + 1
    for c in coords(*d, True):
        cmap_2[c] = cmap_2.get(c, 0) + 1

p1 = sum([1 for c, v in cmap_1.items() if v > 1])
p2 = sum([1 for c, v in cmap_2.items() if v > 1])


print(f'part1: {p1}')
print(f'part1: {p2}')

