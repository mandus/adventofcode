#!/usr/bin/env python3
# Advent of Code 2018 - Åsmund Ødegård

import sys
import re
import numpy as np
import matplotlib.pyplot as plt

lre = re.compile('position=<(.*),(.*)> velocity=<(.*),(.*)>')


def read_input(fn):
    """read and sort input"""
    with open(fn, 'r') as f:
        lines = f.read().splitlines()
    return [map(int, lre.match(l).groups()) for l in lines]


class point:

    def __init__(s, x, y, vx, vy):
        s.point = np.array((x, y))
        s.vel = np.array((vx, vy))

    def x(s):
        return s.point[0]

    def y(s):
        return s.point[1]

    def pos(s, c):
        """Return position at time c"""
        return s.point + c*s.vel

    def __repr__(s):
        return "(%s)[%s]" % (s.point, s.vel)


def boundingbox(points):
    return [
        min(p[0] for p in points),
        max(p[0] for p in points),
        min(p[1] for p in points),
        max(p[1] for p in points)]


def draw_points(points, c):
    pos = [tuple(p.pos(c)) for p in points]
    pos_x = [x for (x, y) in pos]
    pos_y = [y for (x, y) in pos]
    plt.plot(pos_x, pos_y, 'ro')
    bb = [120, 200, 125, 160]
    plt.axis(bb)
    plt.show()


def main(fn):
    data = read_input(fn)
    points = []
    for d in data:
        points.append(point(*d))

    clock = 10005
    while True:
        draw_points(points, clock)

        clock += 1
        print(clock)


if __name__ == '__main__':
    main(sys.argv[1])
