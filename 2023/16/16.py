#!/usr/bin/env python3

fn = 't1.txt'
# fn = 'input.txt'

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


def trace(d: list, p: tuple, v: tuple, m: dict, seen: dict) -> dict:
    """d: data; p: position; v: vec for direction; m: reached cells
       return reached cells

       postions/directions: (row, col).
    """
    # TODO: consider if same p/v should trigger immediate return with known result?

    # If I get to same place with same direction as before; exit - no need to continue
    if (p, v) in seen:
        return m
    seen[(p, v)] = True

    sh = (len(d), len(d[0]))
    nx = add(p, v)
    if out(sh, nx):
        # left grid
        return m
    m[nx] = m.get(nx, 0) + 1
    r, c = nx
    sy = d[r][c]
    if sy == '.' or (sy == '-' and v[0] == 0) or (sy == '|' and v[1] == 0):
        return trace(d, nx, v, m, seen)
    if sy == '-' and v[1] == 0:
        trace(d, nx, (0, 1), m, seen)
        return trace(d, nx, (0, -1), m, seen)
    if sy == '|' and v[0] == 0:
        trace(d, nx, (1, 0), m, seen)
        return trace(d, nx, (-1, 0), m, seen)
    if sy == '/':
        return trace(d, nx, forw(v), m, seen)
    if sy == '\\':
        return trace(d, nx, backw(v), m, seen)
    raise Exception(f'should not get here with {nx} and {v} at {sy}')


d = [[c for c in x] for x in open(fn).read().strip().split('\n')]
m = trace(d, (0, -1), (0, 1), {}, {})

# for r in range(len(d)):
#     for c in range(len(d[0])):
#         print('#' if (r, c) in m else '.', end='')
#     print()

print(f'part1: {len(m)}')
