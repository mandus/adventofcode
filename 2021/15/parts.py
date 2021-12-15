#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'


data = [list(map(int, li.strip())) for li in open(fn).readlines()]


def neighb(p: tuple) -> tuple:
    return [(p[0]+i, p[1]+j) for i, j in [(0, -1), (-1, 0), (1, 0), (0, 1)]]


def bigdata(d: list, i: int, j: int, n: int, m: int) -> int:
    # wrap around with increment for larger datasets (part 2)
    inci = i // n
    incj = j // m
    bi = i - n*inci
    bj = j - m*incj
    return (d[bi][bj] - 1 + inci + incj) % 9 + 1


def updateneighb(d: list, p: tuple, risk: dict, path: dict, n: int, m: int) -> None:
    # recursively update all neighbours if they get updated...
    for nb in neighb(p):
        ni, nj = nb[0], nb[1]
        if nb in risk and bigdata(d, ni, nj, n, m) + risk[p] < risk[nb]:
            risk[nb] = bigdata(d, ni, nj, n, m) + risk[p]
            path[nb] = p
            updateneighb(d, nb, risk, path, n, m)


def findpath(d: list, mul: int) -> int:
    n, m = len(d), len(d[0])
    N, M = mul*n, mul*m

    risk = {(0, 0): 0}
    path = {}

    for i in range(N):
        for j in range(M):
            p = (i, j)
            v = bigdata(d, i, j, n, m)
            c = [(nb, risk[nb] + v) for nb in neighb(p) if nb in risk]
            if c:
                c = sorted(c, key=lambda x: x[1])[0]
                risk[p] = c[1]
                path[p] = c[0]

            updateneighb(d, p, risk, path, n, m)

    return risk[(N-1, M-1)]


print('part1: ', findpath(data, 1))
print('part2: ', findpath(data, 5))
