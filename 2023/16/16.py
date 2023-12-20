#!/usr/bin/env python3

fn = 't1.txt'
fn = 'input.txt'


def add(p, v):
    return (p[0] + v[0], p[1] + v[1])


def out(s, n):
    if 0 <= n[0] < s[0] and 0 <= n[1] < s[1]:
        return False
    return True


def forw(v):
    return {(0, 1): (-1, 0),
            (0, -1): (1, 0),
            (1, 0): (0, -1),
            (-1, 0): (0, 1)}[v]


def backw(v):
    return {(0, 1): (1, 0),
            (0, -1): (-1, 0),
            (1, 0): (0, 1),
            (-1, 0): (0, -1)}[v]


def process(d: list) -> dict:
    m = {}
    seen = {}
    sh = (len(d), len(d[0]))

    nxt = [((0, -1), (0, 1))]
    while nxt:
        p, v = nxt.pop()

        if (p, v) in seen:
            # been here before
            continue
        seen[(p, v)] = True

        # shine the light:
        nx = add(p, v)
        if out(sh, nx):
            # left grid
            continue
        m[nx] = m.get(nx, 0) + 1
        r, c = nx
        sy = d[r][c]
        if sy == '.' or (sy == '-' and v[0] == 0) or (sy == '|' and v[1] == 0):
            nxt.append((nx, v))
            continue
        if sy == '-' and v[1] == 0:
            nxt.extend([(nx, (0, 1)), (nx, (0, -1))])
            continue
        if sy == '|' and v[0] == 0:
            nxt.extend([(nx, (1, 0)), (nx, (-1, 0))])
            continue
        if sy == '/':
            nxt.append((nx, forw(v)))
            continue
        if sy == '\\':
            nxt.append((nx, backw(v)))
    return m


d = [[c for c in x] for x in open(fn).read().strip().split('\n')]
m = process(d)

print(f'part1: {len(m)}')
