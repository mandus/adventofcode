#!/usr/bin/env python3


#  fn = 'test_input.txt'
fn = 'input.txt'


def chk_line(li):
    stack = []
    opening = '({[<'
    closing = ')}]>'
    close_map = {
            ')': '(',
            ']': '[',
            '}': '{',
            '>': '<',
            }

    illegal_cost = {
            ')': 3,
            ']': 57,
            '}': 1197,
            '>': 25137,
            }

    for c in li:
        if c in opening:
            stack.append(c)
        elif c in closing and close_map[c] == stack[-1]:
            stack = stack[:-1]
        else:
            return illegal_cost[c], []
    return 0, stack


def comp_line(stack):
    close_map = {
            '(': 1,
            '[': 2,
            '{': 3,
            '<': 4,
            }
    total = 0
    for c in reversed(stack):
        total *= 5
        total += close_map[c]
    return total


data = [li.strip() for li in open(fn).readlines()]

corrupt_sum = 0
incompletes = []
for line in data:
    corrupt, stack = chk_line(line)
    if corrupt:
        corrupt_sum += corrupt
    else:
        # incomplete
        incompletes.append(comp_line(stack))

print('part1: ', corrupt_sum)
print('part2: ', sorted(incompletes)[len(incompletes)//2])
