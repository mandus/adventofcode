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

    def __init__(self, l, basetime):
        self.name = l
        self.before = []
        self.after = []
        self.dotime = basetime + ord(l) - 64

    def markdone(self, t):
        # remove t if in 'after'
        if t in self.after:
            self.after.pop(self.after.index(t))

    def __repr__(self):
        return "%s: %s" % (self.name, self.after)


class worker():

    def __init__(self, i):
        self.id = i
        self.busy = False
        self.do_task = None

    def idle(self):
        return not self.busy

    def do(self, t, clock):
        self.busy = True
        self.do_task = t
        self.finish_at = clock + t.dotime

    def is_finished(self, clock):
        return self.busy and clock >= self.finish_at

    def finish(self):
        self.busy = False

    def __repr__(self):
        return "%s: %s %s" % (self.id, self.busy, self.do_task)


def main(fn, w, base):
    data = read_input(fn)
    tasks = {}
    starts = {}

    workers = [worker(i) for i in range(int(w))]
    base = int(base)

    for d in data:
        tb = tasks.get(d[0], task(d[0], base))
        ta = tasks.get(d[1], task(d[1], base))

        tb.before.append(ta)
        ta.after.append(tb)

        tasks[d[0]] = tb
        tasks[d[1]] = ta

        for t in (ta, tb):
            starts[t.name] = len(t.after) == 0

    dolist = [k for k, v in starts.items() if v]

    assigned = []
    done = []
    clock = 0
    while dolist or assigned:
        dolist.sort()

        # any finished tasks?
        finish = [w for w in assigned if w.is_finished(clock)]
        while finish:
            w = finish.pop()
            w.finish()
            t_done = w.do_task
            assigned.pop(assigned.index(w))
            done.append(t_done.name)

            # find next cands:
            for t in t_done.before:
                t.markdone(t_done)
                if len(t.after) == 0:
                    dolist.append(t.name)

        # do we have idle workers?
        w_avail = [w for w in workers if w.idle()]

        while w_avail and dolist:
            w = w_avail.pop(0)
            t0 = dolist.pop(0)
            t_do = tasks[t0]
            w.do(t_do, clock)
            assigned.append(w)

        clock += 1

    # part 1
    print("".join(done))
    # part 2
    print('clock when done:', clock-1)


if __name__ == '__main__':
    main(*sys.argv[1:])
