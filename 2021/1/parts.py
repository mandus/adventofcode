#!/usr/bin/env python3

nums = [int(l.strip()) for l in open('input.txt', 'r').readlines()]

part1 = sum([y - x > 0 for x, y in zip(nums, nums[1:] + [nums[-1]])])
print(f'part1: {part1}')

part2 = sum([(-sum(nums[i:i+3])+sum(nums[i+1:i+4])) > 0 for i in range(len(nums)-3)])
print(f'part2: {part2}')