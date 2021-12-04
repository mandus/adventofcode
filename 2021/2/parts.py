#!/usr/bin/env python3

part1 = ''
part2 = ''

fn = 'input.txt'

data = [line.strip().split() for line in open(fn, 'r')]

ops = {
    'forward': lambda v, p, d: (p+v, d),
    'up': lambda v, p, d: (p, d-v),
    'down': lambda v, p, d: (p, d+v),
    }

p, d = 0, 0
for op, val in data: p, d = ops[op](int(val), p, d)


part1 = p*d

print(f'part1: {part1}')

ops = {
    'forward': lambda v, p, d, a: (p+v, d+(a*v), a),
    'up': lambda v, p, d, a: (p, d, a-v),
    'down': lambda v, p, d, a: (p, d, a+v),
    }

p, d, aim = 0, 0, 0
for op, val in data: p, d, aim = ops[op](int(val), p, d, aim)

part2 = p*d
print(f'part2: {part2}')
