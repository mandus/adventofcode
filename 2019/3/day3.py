#!/usr/bin/env python
# coding: utf-8

data = [l.strip().split(",") for l in open("input.txt").readlines()]

dirs = {'R': 1, 'U': 1, 'L': -1, 'D': -1}
axis = {'R': 0, 'U': 1, 'L': 0, 'D': 1}


points = {}
def segment_points(points, data):
    # do not track self-intersection - require two-pass...
    lp = {}
    pos = [0, 0]
    total_distance = 0
    for segment in data:
        d, l = (segment[:1], int(segment[1:]))
        sign = dirs[d]
        ax = axis[d]
        for i in range(l):
            pos[ax] += sign
            total_distance += 1
            tp = tuple(pos)
            # track total distance, but skip insert if already present (self-intersection)
            lp[tp] = min(lp.get(tp, total_distance), total_distance)
    for k, v in lp.items():
        points[k] = points.get(k, 0)+1
    return lp


dist_a = segment_points(points, data[0])
dist_b = segment_points(points, data[1])


# part 1
print("part1:", min([abs(p[0])+abs(p[1]) for p, v in points.items() if v > 1]))


# find total-dists to crossings:
# part 2
print("part2:", min([dist_a[p]+dist_b[p] for p, v in points.items() if v > 1]))
