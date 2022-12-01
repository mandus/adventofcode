#!/usr/bin/env python3

nums = [line.strip() for line in open('input.txt', 'r')]


def agg(nums):
    acc = 0
    vals = []
    for num in nums:
        if num == '':
            vals.append(acc)
            acc = 0
        else:
            acc += int(num)
    return sorted(vals)


def solve(n, cnt):
    return sum(n[-cnt:])


a_n = agg(nums)
print('part1: ', solve(a_n, 1))
print('part2: ', solve(a_n, 3))
