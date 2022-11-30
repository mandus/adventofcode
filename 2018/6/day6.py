#!/usr/bin/env python3
# Advent of Code 2018 - Åsmund Ødegård

import sys
import operator


def readpts(fn):
    with open(fn, 'r') as f:
        def coord(x): return (int(x[0]), int(x[1]))
        pts = [coord(p.split(', ')) for p in f.read().splitlines()]
    pts.sort(key=operator.itemgetter(1, 0))
    return pts


def dist(p1, p2):
    return abs(p2[0]-p1[0])+abs(p2[1]-p1[1])


def main(fn):
    pts = readpts(fn)

    xmin = min([p[0] for p in pts])
    ymin = min([p[1] for p in pts])
    xmax = max([p[0] for p in pts])
    ymax = max([p[1] for p in pts])

    assoc = {}
    sumdists = {}
    for i in range(xmin, xmax+1):
        for j in range(ymin, ymax+1):
            cands = [(p, dist((i, j), p)) for p in pts]

            # part 2
            sumdists[(i, j)] = sum([p[1] for p in cands])

            mds = min([p[1] for p in cands])
            cands = [p for p in cands if p[1] == mds]
            if len(cands) == 1:
                assoc[(i, j)] = cands[0][0]

    revmap = {}
    for k, v in assoc.items():
        li = revmap.get(v, [])
        li.append(k)
        revmap[v] = li

    # part 1
    print(max([len(v) for k, v in revmap.items()]))

    # part 2
    print(len([p for p, s in sumdists.items() if s < 10000]))


if __name__ == '__main__':
    fn = sys.argv[1]
    main(fn)
