#!/usr/bin/env python3

fn = 'input.txt'
fn = 't1.txt'
d = open(fn).read().strip().split('\n\n')


def i(d):
    return [int(x) for x in d.split(':')[1].strip().split()]


def i2(d):
    s = [int(x) for x in d.split(':')[1].strip().split()]
    pairs = []
    for i in range(len(s)//2):
        pairs.append((s[i*2], s[i*2+1]))
    return pairs


def sec(s, d):
    """state s, section data d"""
    d = [[int(y) for y in x.split()] for x in d.split('\n')[1:]]
    m = {i: i for i in s}
    for r in d:
        dest, src, ln = r
        in_range = [i for i in s if src <= i < src+ln]
        m.update({i: dest + (i - src) for i in in_range})
    return [m[i] for i in s]


def movepair(p, d):
    print('pair:', p)
    d = [[int(y) for y in x.split()] for x in d.split('\n')[1:]]
    print('rules:', d)


s = i(d[0])
# run through each of the reminding paragraphs
for p in d[1:]:
    s = sec(s, p)
print(f'part1: {min(s)}')

pairs = i2(d[0])
print(pairs)
for p in pairs:
    movepair(p, d[1:][0])
# print(f'part2: {min()}')
