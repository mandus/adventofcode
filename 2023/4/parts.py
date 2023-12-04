#!/usr/bin/env python3

import math

fn = 't1.txt'
fn = 'input.txt'
d = open(fn).read().strip().split('\n')


def score(li, p2=False):
    n = li[8:]
    w, m = n.split('|')
    w = w.strip().split()
    m = m.strip().split()
    c = set(w).intersection(m)
    if p2:
        return len(c)
    score = math.pow(2, len(c)-1) if len(c) > 0 else 0
    return int(score)


def counts(d):
    cc = {i: 1 for i in range(len(d))}
    for i, li in enumerate(d):
        cur = cc[i]
        acc = score(li, True)
        for j in range(i+1, i+1+acc):
            cc[j] = cc[j]+cur
    return cc


print(f'part1: {sum([score(li) for li in d])}')
print(f'part2: {sum([v for _, v in counts(d).items()])}')
