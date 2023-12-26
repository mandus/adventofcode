#!/usr/bin/env python3

fn = 't1.txt'


def ins(sh, p):
    return 0 <= p[0] < sh[0] and 0 <= p[1] <= sh[1]


def hl(d: list, sh, tuple, S: tuple, E: tuple) -> int:
    # TODO: In seen we need to maintain minimal heat loss at each point
    # but also how we got there since there may be more than one way to reach
    # each cell due to the three steps one direction rule
    se = {}
    # points in queue: position, direction, num steps, current heat loss
    qu = [(S, (0, 1), -1, 0), (S, (1, 0), -1, 0)]

    while qu:

    return 0


def p1(d: list) -> int:
    sh = len(d), len(d[0])
    S = (0, 0)
    E = (sh[0]-1, sh[1]-1)
    return hl(d, sh, S, E)


d = [[c for c in x] for x in open(fn).read().split()]
print(f'part1: {p1(d)}')
