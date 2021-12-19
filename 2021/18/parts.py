#!/usr/bin/env python3

from typing import Union
import math
from functools import reduce


# Snailfish math - addition, and reduction by explode and split

def trsf(c: chr) -> Union[chr, int]:
    if c in '[]':
        return c
    elif c in ', ':
        return '!'  # makes it easier to filter
    else:
        return int(c)


def tokens(s: str) -> list:
    return list(filter(lambda x: x != '!', map(trsf, s)))


def tokenlst(n: list) -> list:
    # de-tokenize; i.e. make it into nested python lists
    s = ""
    for e in n:
        if intp(e):
            s += f'{e}, '
        elif e == '[':
            s += e
        elif e == ']':
            s += f'{e},'
    return eval(s.strip(','))


def intp(e: Union[chr, int]) -> bool:
    return type(e) == int


def adjdpt(e: chr, dpt: int) -> int:
    if e == '[':
        return dpt + 1
    elif e == ']':
        return dpt - 1
    return dpt


def explode(n: list) -> tuple:
    ret = list(n)
    dpt = 0
    lftrg = None
    rgthrg = None
    expl = False
    stepout = False
    for idx, e in enumerate(n):
        dpt = adjdpt(e, dpt)
        if stepout and dpt < 5:
            stepout = False
        if intp(e) and dpt < 5 and not expl:
            lftrg = idx
        if dpt == 5 and not expl:
            expl = idx
            stepout = True
        if intp(e) and expl and not stepout:
            rgthrg = idx
        if rgthrg and expl:
            break
    if expl:
        if lftrg:
            ret[lftrg] += n[expl + 1]
        if rgthrg:
            ret[rgthrg] += n[expl + 2]
        ret = ret[:expl] + [0] + ret[expl+4:]

    return ret, expl


def split(n: list) -> tuple:
    ret = list(n)
    spltidx = False
    for idx, e in enumerate(n):
        if intp(e) and e > 9:
            spltidx = idx
            break
    if spltidx:
        val = n[spltidx]
        ret = n[:spltidx] + ['['] + [math.floor(val/2), math.ceil(val/2)] + [']'] + n[spltidx+1:]

    return ret, spltidx


def snailreduce(n: list) -> list:
    done = False
    while not done:
        n, expl = explode(n)
        if expl:
            continue  # start over again
        n, splt = split(n)
        if splt:
            continue  # start over again

        done = not (expl or splt)
    return n


def add(a: list, b: list) -> list:
    return snailreduce(['['] + a + b + [']'])


def magnitude(n: list) -> int:
    left, right = n
    if intp(left):
        left = 3 * left
    else:
        left = 3 * magnitude(left)
    if intp(right):
        right = 2 * right
    else:
        right = 2 * magnitude(right)
    return left + right


# fn = 'test_input.txt'
# fn = 'test_input2.txt'
fn = 'input.txt'


data = list(map(tokens, [li.strip() for li in open(fn).readlines()]))
print('part1 ', magnitude(tokenlst(reduce(add, data))))

high = 0
for a in data:
    for b in data:
        high = max(high, magnitude(tokenlst(add(a, b))))
        high = max(high, magnitude(tokenlst(add(b, a))))

print('part2 ', high)

#
# Tests
#


def test_tokenlst():
    assert tokenlst(tokens('[[[1, 2], 3], 4]')) == [[[1, 2], 3], 4]


def test_explode():
    # first test no-op
    assert explode(tokens('[[[1, 2], 3], 4]')) == (tokens('[[[1, 2], 3], 4]'), False)
    # then test some explosions
    assert explode(tokens('[[[[[9, 8], 1], 2], 3], 4]')) == (tokens('[[[[0, 9], 2], 3], 4]'), 4)
    assert explode(tokens('[7,[6,[5,[4,[3,2]]]]]')) == (tokens('[7,[6,[5,[7,0]]]]'), 8)
    assert explode(tokens('[[6,[5,[4,[3,2]]]],1]')) == (tokens('[[6,[5,[7,0]]],3]'), 7)
    assert explode(tokens('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]')) \
           == (tokens('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]'), 7)
    assert explode(tokens('[[[[[1,1],[2,2]][3,3]],[4,4]],[5,5]]')) == (tokens('[[[[0,[3,2]],[3,3]],[4,4]],[5,5]]'), 4)

    # chain two explodes
    assert explode(explode(tokens('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]'))[0])[0] \
           == tokens('[[3,[2,[8,0]]],[9,[5,[7,0]]]]')


def test_split():
    # first test no-op
    assert split(tokens('[[[1, 2], 3], 4]')) == (tokens('[[[1, 2], 3], 4]'), False)
    # then test some simple splits
    assert split(
        ['[', '[', '[', '[', 0, 7, ']', 4, ']', '[', 15, '[', 0, 13, ']', ']', ']', '[', 1, 1, ']', ']']) \
        == (
        ['[', '[', '[', '[', 0, 7, ']', 4, ']', '[', '[', 7, 8, ']', '[', 0, 13, ']', ']', ']', '[', 1, 1, ']', ']'],
        10)
    assert split(split(
        ['[', '[', '[', '[', 0, 7, ']', 4, ']', '[', 15, '[', 0, 13, ']', ']', ']', '[', 1, 1, ']', ']'])[0]) \
        == (
        ['[', '[', '[', '[', 0, 7, ']', 4, ']', '[', '[', 7, 8, ']',
         '[', 0, '[', 6, 7, ']', ']', ']', ']', '[', 1, 1, ']', ']'],
        16)


def test_add():
    assert add(tokens('[1, 2]'), tokens('[3, 4]')) == tokens('[[1, 2],[3, 4]]')
    assert add(tokens('[1,2]'), tokens('[[3,4],5]')) == tokens('[[1,2],[[3,4],5]]')
    assert add(tokens('[[[[4,3],4],4],[7,[[8,4],9]]]'), tokens('[1,1]')) == tokens('[[[[0,7],4],[[7,8],[6,0]]],[8,1]]')

    assert reduce(add, map(tokens, ['[1,1]', '[2,2]', '[3,3]', '[4,4]'])) == tokens('[[[[1,1],[2,2]],[3,3]],[4,4]]')
    print(reduce(add, map(tokens, ['[1,1]', '[2,2]', '[3,3]', '[4,4]', '[5,5]'])))
    assert reduce(add, map(tokens, ['[1,1]', '[2,2]', '[3,3]', '[4,4]', '[5,5]'])) \
           == tokens('[[[[3,0],[5,3]],[4,4]],[5,5]]')


def test_magnitude():
    assert magnitude([9, 1]) == 29
    assert magnitude(tokenlst(tokens('[[9,1],[1,9]]'))) == 129
