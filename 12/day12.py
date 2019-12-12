#!/usr/bin/env python3

import numpy as np


moons = [np.array((17, 5, 1)), np.array((-2, -8, 8)), np.array((7, -6, 14)), np.array((1, -10, 4))]
n = len(moons)

for i in range(n):
    for j in range(n):
        if i == j:
            continue
        diff = moons[i]-moons[j]
        diff/np.abs(diff)
