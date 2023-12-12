#!/usr/bin/env python3

fn = 't1.txt'
# fn = 'input.txt'

d = open(fn).read().strip().split('\n')
x = [[c for c in ln] for ln in d]

print(f'part1: {1}')
print(f'part2: {2}')
