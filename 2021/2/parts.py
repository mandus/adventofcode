#!/usr/bin/env python3

part1 = ''
part2 = ''


# data = [
#     "forward 5",
#     "down 5",
#     "forward 8",
#     "up 3",
#     "down 8",
#      "forward 2"]

data = [l.strip() for l in open('input.txt', 'r').readlines()]

p, d = 0, 0
ops = {
    'forward': lambda v, p, d: (p+v, d),
    'up': lambda v, p, d: (p, d-v),
    'down': lambda v, p, d: (p, d+v),
    }

for line in data:
    op, val = line.split()
    val = int(val)
    #print(f'{op} > {val} >', ops[op](val, p, d))
    p, d = ops[op](val, p, d)

part1 =  p*d

print(f'part1: {part1}')


p, d, aim = 0, 0, 0
ops = {
    'forward': lambda v, p, d, a: (p+v, d+(a*v), a),
    'up': lambda v, p, d, a: (p, d, a-v),
    'down': lambda v, p, d, a: (p, d, a+v),
    }

for line in data:
    op, val = line.split()
    val = int(val)
    #print(f'{op} > {val} >', ops[op](val, p, d, aim))
    p, d, aim = ops[op](val, p, d, aim)

part2 =  p*d
print(f'part2: {part2}')