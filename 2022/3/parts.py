#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'

d = open(fn).read().strip().split('\n')
a = ord('a')
A = ord('A')

def half(l):
    h = len(l)//2
    return (set(l[:h]), set(l[h:]))


def chrnum(c):
    cv = ord(c)
    if cv < a:
        return cv - A + 27
    return cv - a + 1


dh = [half(l) for l in d]
both = [list(l.intersection(r))[0] for (l, r) in dh]
print('part1 ', sum([chrnum(c) for c in both]))


# intersection, groups of 3
prio = 0
for i in range(len(d)//3):
    b = 3*i
    sub = d[b:b+3]
    prio += chrnum(list(set.intersection(*map(set, sub)))[0])
print('part2 ', prio)
