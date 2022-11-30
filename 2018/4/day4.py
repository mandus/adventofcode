#!/usr/bin/env python3

import sys
import re

guard_re = re.compile('.*Guard (.*) begins.*')
asleep_re = re.compile('.*00:(.*)] falls asleep.*')
awake_re = re.compile('.*00:(.*)] wakes up.*')


def read_input(fn):
    """read and sort input"""
    with open(fn, 'r') as f:
        data = f.read().splitlines()
    data.sort(key=lambda x: x.split()[0:2])
    return data


def process_lines(lines):
    guards = {}

    guard = None
    start_sleep = None
    for l in lines:

        g = guard_re.match(l)
        a = asleep_re.match(l)
        w = awake_re.match(l)

        if g:
            guard = g.groups()[0]
            if guard not in guards:
                guards[guard] = [0 for i in range(60)]
        elif a:
            start_sleep = int(a.groups()[0])
        elif w:
            end_sleep = int(w.groups()[0])
            for i in range(start_sleep, end_sleep):
                guards[guard][i] += 1
            start_sleep = None

    # if in sleep at the end (last line is sleep, and never awake),
    # include that
    if start_sleep:
        for i in range(start_sleep, 60):
            guards[guard][i] += 1

    return guards


def main(fn):
    guards = process_lines(read_input(fn))

    heavysleeper = None
    cur_max_slept = 0
    idx = -1

    single_minute_max = -1
    single_minute_guard = None

    for g, sleep in guards.items():
        # part 1
        slept = sum(sleep)
        if cur_max_slept < slept:
            cur_max_slept = slept
            heavysleeper = g
            idx = sleep.index(max(sleep))
        # part 2
        if max(sleep) > single_minute_max:
            single_minute_max = max(sleep)
            single_minute_guard = g

    print('heavy sleeper is', heavysleeper, '(', cur_max_slept, ')')
    print('sleepy minute:', idx)
    print('checksum: ', int(heavysleeper[1:])*idx)

    print('\nsingle minute max', single_minute_max)
    max_minute = guards[single_minute_guard].index(single_minute_max)
    print('in minute', max_minute)
    print('of guard', single_minute_guard)
    print('checksum: ', int(single_minute_guard[1:])*max_minute)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Missing filename')
        sys.exit(1)
    main(sys.argv[1])
