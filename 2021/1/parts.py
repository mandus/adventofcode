#!/usr/bin/env python3

nums = [int(line) for line in open('input.txt', 'r')]

part1 = sum([y - x > 0 for x, y in zip(nums[:-1], nums[1:])])
print(f'part1: {part1}')

part2 = sum([y - x > 0 for x, y in zip(nums[:-3], nums[3:])])
print(f'part2: {part2}')
