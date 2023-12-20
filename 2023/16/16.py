#!/usr/bin/env python3

fn = 't1.txt'
fn = 'input.txt'


def add(p, v):
    return (p[0] + v[0], p[1] + v[1])


def out(s, n):
    return not (0 <= n[0] < s[0] and 0 <= n[1] < s[1])


def forw(v):
    return (-v[1], -v[0])


def backw(v):
    return (v[1], v[0])


def process(d: list, pst: tuple = (0, -1), vst: tuple = (0, 1)) -> dict:
    m = {}
    seen = {}
    sh = (len(d), len(d[0]))

    nxt = [(pst, vst)]
    while nxt:
        p, v = nxt.pop()

        if seen.get((p, v)):
            continue
        seen[(p, v)] = True

        # shine the light:
        nx = add(p, v)
        if out(sh, nx):
            continue

        m[nx] = m.get(nx, 0) + 1
        r, c = nx
        sy = d[r][c]
        if sy == '.' or (sy == '-' and v[0] == 0) or (sy == '|' and v[1] == 0):
            nxt.append((nx, v))
        elif sy == '-' and v[1] == 0:
            nxt.extend([(nx, (0, 1)), (nx, (0, -1))])
        elif sy == '|' and v[0] == 0:
            nxt.extend([(nx, (1, 0)), (nx, (-1, 0))])
        elif sy == '/':
            nxt.append((nx, forw(v)))
        elif sy == '\\':
            nxt.append((nx, backw(v)))
    return m


def p1(d):
    return len(process(d))


def p2(d):
    r, c = len(d), len(d[0])
    assert r == c

    es = []
    for i in range(r):
        es.append(len(process(d, (i, -1), (0, 1))))
        es.append(len(process(d, (i, c), (0, -1))))
        es.append(len(process(d, (-1, i), (1, 0))))
        es.append(len(process(d, (r, i), (-1, 0))))

    return max(es)


d = [[c for c in x] for x in open(fn).read().strip().split('\n')]
print(f'part1: {p1(d)}')
print(f'part2: {p2(d)}')
