#!/usr/bin/env python3
# Advent of Code 2018 - Åsmund Ødegård
# didn't felt like struggling with part2, so looked at answers..
# Main idea: after some generation, the sumpots will increase
# with a given number for the rest of the generations (186 for each gen
# after 100 in my case)
#
# it's some fixpoint so should be possible to show mathematically...

import sys


def readlines(fn):
    with open(fn, 'r') as f:
        lines = f.read().splitlines()
    init = lines[0][len("initial state: "):]

    rules = {}
    for line in lines[2:]:
        spl = line.split(' => ')
        rules[spl[0]] = spl[1]

    # We will always need 4 dots before the first #, in case there is
    # a ....# => # rule, and 4 dots after the last #, for a #.... => # rule
    init = "...." + init + "...."
    return init, rules


def main(fn, lastgen='20'):
    lastgen = int(lastgen)
    sz = 5
    init, rules = readlines(fn)
    # pots = len([i for i in init if i == '#'])
    baseidx = -4  # added 4 dots
    # print('0:', init, '(%d)' % pots)

    prev = 0
    for gen in range(1, lastgen+1):
        nextline = list(init[:4])
        for i in range(len(init)+1-sz):
            pat = str(init[i:i+sz])
            nextline.append(pat[-1])
            if pat in rules:
                # print('apply', pat, '=>', rules[pat])
                # print(pat)
                nextline[i+2] = rules[pat]
            else:
                # no match means there shoud be no plant in the
                # middle pot next gen:
                nextline[i+2] = '.'
        # pts = len([i for i in nextline if i == '#'])
        # print('%s:' % gen, ''.join(nextline), '(%d)' % pts)
        # pots += pts
        init = ''.join(nextline)
        # There should always be at least 4 dots in the begining
        while init[:4] != '....':
            init = '.' + init
            baseidx -= 1
        # and at the end
        while init[-4:] != '....':
            init += '.'
        sumpots = sum([i+baseidx for i in range(len(init)) if init[i] == '#'])
        diff = sumpots - prev
        print(gen, sumpots, diff)
        prev = sumpots


if __name__ == '__main__':
    main(*sys.argv[1:])
