#!/usr/bin/env python3

fn = 't1.txt'
fn = 'input.txt'
d = open(fn).read().strip().split('\n')

# idea: for each number, store coord digits as a list; (2,1), (2,2), (2,3)
# for symbols (not digit, not dot), store both coord and adjacents; e.g. (4,4)
# as well as (3,3), (3,4), (3,5), (4,3), (4,5), (5,3), (5,4), (5,5)
#
# when I have all these lists, and I can for each digit check if there is an
# intersection with one of the symbols-coordlists; if so it's a part


def isdig(c: str) -> bool:
    return c in '0123456789'


def isdot(c: str) -> bool:
    return c == '.'


def isstar(c: str) -> bool:
    return c == '*'


def adjc(i: int, j: int) -> set:
    return [(x, y) for x in range(i-1, i+2) for y in range(j-1, j+2)]


def p_l(l: str, i: int) -> (list, list, list):
    """parse line for row i"""
    numbers = []
    symbols = []
    stars = []
    in_num = False
    num = ''
    num_coor = []
    for j, c in enumerate(l):
        if isdig(c):
            num += c
            num_coor.append((i, j))
            if not in_num:
                in_num = True
        elif isdot(c):
            if in_num:
                # finish number:
                in_num = False
                numbers.append((int(num), set(num_coor)))
                num = ''
                num_coor = []
        else:
            # must be a symbol
            a = adjc(i, j)
            symbols.extend(a)
            if in_num:
                in_num = False
                numbers.append((int(num), set(num_coor)))
                num = ''
                num_coor = []
            if isstar(c):
                stars.append(set(a))
    if in_num:
        # line ending with numbers
        numbers.append((int(num), set(num_coor)))

    return numbers, symbols, stars


def parse(d: list) -> (list, list):
    numbers = []
    symbols = []
    stars = []
    for i, line in enumerate(d):
        n, s, st = p_l(line, i)
        if n:
            numbers.extend(n)
        if s:
            symbols.extend(s)
        if st:
            stars.extend(st)
    return numbers, set(symbols), stars


def is_part(n_c: set, s_c: set) -> bool:
    return n_c.intersection(s_c)


nums, syms, stars = parse(d)
parts = [n for n, c in nums if is_part(c, syms)]
print(f'part1: {sum(parts)}')

gears = []
for st in stars:
    gc = [n for n, c in nums if is_part(c, st)]
    if len(gc) == 2:
        gears.append(gc[0]*gc[1])

print(f'part2: {sum(gears)}')
