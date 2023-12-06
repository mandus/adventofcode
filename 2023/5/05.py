#!/usr/bin/env python3

fn = 'input.txt'
# fn = 't1.txt'
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


def movepairs(p, d):
    p.sort()
    rs = [[int(y) for y in x.split()] for x in d.split('\n')[1:]]
    rs = sorted(rs, key=lambda x: x[1])
    srcs = list(reversed(p))
    dests = []
    while srcs:
        s, ln = srcs.pop()
        if ln == 0:
            # ignore zero-length ranges
            continue
        handled = False
        for dest, src, rln in rs:
            if s >= src+rln:
                # rule exhausted
                continue
            if s < src and s+ln >= src:
                dests.append((s, src-s))
                delta = ln-src+s
                if delta:
                    dests.append((dest, min(rln, delta)))
                if delta > rln:
                    srcs.append((src+rln, delta-rln))
                handled = True
                break
            elif s >= src and s < src+rln:
                delta = s-src
                dests.append((dest+delta, min(rln-delta, ln)))
                if ln > rln-delta:
                    srcs.append((src+rln, ln-rln+delta))
                handled = True
                break
            elif src >= s+ln:
                # src >= s+ln -> no owerlap, and further rules are higher
                dests.append((s, ln))
                handled = True
                break
        # all rules exhausted; moving forward
        if not handled:
            dests.append((s, ln))
        srcs = list(reversed(srcs))
    return dests


s = i(d[0])
# run through each of the reminding paragraphs
for p in d[1:]:
    s = sec(s, p)
print(f'part1: {min(s)}')

pairs = i2(d[0])
for p in d[1:]:
    pairs = movepairs(pairs, p)
print(f'part2: {sorted(pairs)[0][0]}')
