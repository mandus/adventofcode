#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'

d = open(fn).read().strip().split('\n')
a = ord('a')
A = ord('A')


def splhalf(line):
    h = len(line)//2
    return (set(line[:h]), set(line[h:]))


def chrnum(c):
    cv = ord(c)
    if cv < a:
        return cv - A + 27
    return cv - a + 1


# dh = [splhalf(line) for line in d]
# both = [set.intersection(*parts).pop() for parts in dh]
# print('part1 ', sum([chrnum(c) for c in both]))
#
# take 2 >  no need for all those variables:
print('part 1', sum(
    [chrnum(set.intersection(*parts).pop()) for
     parts in [splhalf(line) for line in d]]))


# intersection, groups of 3
print('part2 ', sum(
    [chrnum(set.intersection(*map(set, d[3*i:3*(i+1)])).pop())
     for i in range(len(d)//3)]))
