#!/usr/bin/env python3


part1 = ''
part2 = ''

# fn = 'test_input.txt'
fn = 'input.txt'


def btod(b):
    return sum(v*(2**i) for i, v in enumerate(reversed(b)))


data = [[int(s) for s in l.strip()] for l in open(fn, 'r').readlines()]

cnt_2 = len(data)/2
gamma = [sum([d[i] for d in data]) > cnt_2 for i in range(len(data[0]))]
eps = [sum([d[i] for d in data]) < cnt_2 for i in range(len(data[0]))]

part1 = btod(gamma)*btod(eps)
print(f'part1: {part1}')

ox = list(data)
idx = 0
while len(ox) > 1:
    cnt = len(ox)/2
    mcv = int(sum([d[idx] for d in ox]) >= cnt)
    ox = [o for o in ox if o[idx] == mcv]
    idx += 1

csr = list(data)
idx = 0
while len(csr) > 1:
    cnt = len(csr)/2
    lcv = int(sum([d[idx] for d in csr]) < cnt)
    csr = [c for c in csr if c[idx] == lcv]
    idx += 1

part2 = btod(ox[0])*btod(csr[0])
print(f'part2: {part2}')
