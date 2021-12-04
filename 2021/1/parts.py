#!/usr/bin/env python3

nums = [int(line) for line in open('input.txt', 'r')]


def solve(d, skp):
    return sum([y - x > 0 for x, y in zip(d[:-skp], d[skp:])])


print('part1: ', solve(nums, 1))
print('part2: ', solve(nums, 3))
