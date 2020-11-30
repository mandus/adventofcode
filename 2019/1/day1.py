#!/usr/bin/env python3

import numpy as np

with open('input.txt') as f:
    data = f.readlines()

nums = np.genfromtxt(data)

fuels = np.vectorize(lambda f: np.floor(f/3)-2)


def fuelsrec(f, acc=0):
    f = fuels(f)
    if f > 0:
        return fuelsrec(f, acc+f)
    return acc


vfuels = np.vectorize(fuelsrec)

part1 = int(np.sum(fuels(nums)))
part2 = int(np.sum(vfuels(nums)))

print(part1)
print(part2)
