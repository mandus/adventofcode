#!/usr/bin/env python3

fn = 't1.txt'
fn = 't2.txt'
fn = 'input.txt'


def hash(s: str) -> int:
    asc = [ord(c) for c in s]
    v = 0
    for i in asc:
        v += i
        v *= 17
        v = v % 256
    return v


d = open(fn).read().strip().split(',')
print(f'part1: {sum([hash(s) for s in d])}')
