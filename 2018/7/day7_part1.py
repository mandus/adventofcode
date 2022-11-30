#!/usr/bin/env python3
# Advent of Code 2018 - Åsmund Ødegård

import sys
import re

lre = re.compile('Step (.*) must .+ step (.*) can.+')


def read_input(fn):
    """read and sort input"""
    with open(fn, 'r') as f:
        lines = f.read().splitlines()
    return [lre.match(l).groups() for l in lines]


class task():

    def __init__(self, l):
        self.name = l
        self.before = []
        self.after = []

    def markdone(self, t):
        # remove t if in 'after'
        if t in self.after:
            self.after.pop(self.after.index(t))

    def __repr__(self):
        return "%s: %s" % (self.name, self.after)


def main(fn):
    data = read_input(fn)
    tasks = {}
    starts = {}

    for d in data:
        tb = tasks.get(d[0], task(d[0]))
        ta = tasks.get(d[1], task(d[1]))

        tb.before.append(ta)
        ta.after.append(tb)

        tasks[d[0]] = tb
        tasks[d[1]] = ta

        for t in (ta, tb):
            starts[t.name] = len(t.after) == 0

    dolist = [k for k, v in starts.items() if v]

    order = []

    while dolist:
        dolist.sort()
        t0 = dolist.pop(0)
        order.append(t0)

        # find next cands:
        t_do = tasks[t0]
        for t in t_do.before:
            t.markdone(t_do)
            if len(t.after) == 0:
                dolist.append(t.name)

    # part 1
    print("".join(order))


if __name__ == '__main__':
    main(sys.argv[1])
