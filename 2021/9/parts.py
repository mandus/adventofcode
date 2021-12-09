#!/usr/bin/env python3

from functools import reduce

#  fn = 'test_input.txt'
fn = 'input.txt'

data = [[int(c) for c in li.strip()] for li in open(fn).readlines()]
rows = len(data)
cols = len(data[0])


def lowpoint(d, i, j, ro, co):
    p = d[i][j]
    adjs = [[i, j-1], [i-1, j], [i+1, j], [i, j+1]]
    return all(d[a[0]][a[1]]-p > 0 for a in adjs if 0 <= a[0] <= ro-1 and 0 <= a[1] <= co-1)


def higherpoints(d, i, j, ro, co):
    p = d[i][j]
    adjs = [[i, j-1], [i-1, j], [i+1, j], [i, j+1]]
    return [(a[0], a[1]) for a in adjs
            if 0 <= a[0] <= ro-1 and 0 <= a[1] <= co-1 and d[a[0]][a[1]] >= p and d[a[0]][a[1]] < 9]


def basin(d, i, j, ro, co):
    # Find points around (i,j) that is higher, but not 9
    search = [(i, j)]
    b = [(i, j)]
    while search:
        s = search[0]
        search = search[1:]
        extra = higherpoints(d, s[0], s[1], ro, co)
        search.extend([p for p in extra if p not in search and p not in b])
        b.extend([p for p in extra if p not in b])
    return b


lowpoints = [(i, j) for i in range(rows) for j in range(cols) if lowpoint(data, i, j, rows, cols)]
print('part1: ', sum(data[i][j]+1 for i, j in lowpoints))
print('part2: ', reduce(lambda x, y: x*y, sorted([len(basin(data, p[0], p[1], rows, cols)) for p in lowpoints])[-3:]))
