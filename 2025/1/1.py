#!/usr/bin/env python3

import sys
import math


def parse(e):
    s = -1 if e[0] == "L" else 1
    return s*int(e[1:])


def sign(x):
    return int(math.copysign(1, x))


fn = sys.argv[1] if sys.argv[1:] else "input.txt"

with open(fn) as f:
    d = f.read().strip().split()


n = [parse(e) for e in d]

p = 50
z = 0
for c in n:
    p = (p + c) % 100
    if p == 0:
        z += 1
print(f"part 1: {z}")


p = 50
z = 0
for c in n:
    z += abs(c) // 100  # normalize rotations
    c = sign(c)*(abs(c) % 100)
    np = p + c
    if np <= 0 and p > 0:
        z += 1
    elif np >= 100:
        z += 1
    p = np % 100
print(f"part 2: {z}")
