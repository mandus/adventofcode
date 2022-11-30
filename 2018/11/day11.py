#!/usr/bin/env python3

import sys
import numpy as np
import numpy.lib as npl


def fuelfu(inp):
    # capture input value
    serialno = inp

    def fu(i, j):
        # numpy array index is zero-based
        x = i+1
        y = j+1
        rackId = x + 10
        stPow = ((rackId * y) + serialno)*rackId
        digit = (stPow - (stPow // 1000)*1000) // 100
        return digit - 5
    return fu


def fuelgrid(inp, mr):
    return np.fromfunction(fuelfu(inp), (mr, mr), dtype=int)


def subgridfu(fg, mr, size):
    return [(i+1, j+1, sum(sum(fg[i:i+size, j:j+size]))) for i in range(mr-size+1) for j in range(mr-size+1)]


def idx(grid, i, j):
    """The resulting grid is zero-base indexed"""
    return grid[i-1, j-1]


def main(inp, mr=300):
    fg = fuelgrid(inp, mr)

    if inp == 8:
        print(idx(fg, 3, 5))
    elif inp == 57:
        print(idx(fg, 122, 79))
    elif inp == 39:
        print(idx(fg, 217, 196))
    elif inp == 71:
        print(idx(fg, 101, 153))

    # part 1
    size = 3
    subgrids = subgridfu(fg, mr, size)
    # print(subgrids)
    print(max(subgrids, key=lambda x: x[2]))

    # part 2 improved
    isz = np.dtype(int).itemsize
    maxmax = 0
    for size in range(1, mr+1):
        patches = npl.stride_tricks.as_strided(fg, shape=(mr-size+1, mr-size+1, size, size), strides=2*(mr*isz, isz))
        windows = patches.sum(axis=(-1, -2))
        sizemax = windows.ravel().max()
        if sizemax > maxmax:
            maxmax = sizemax
            location = np.where(windows == maxmax)
            print(maxmax, ':', location[0][0]+1, location[1][0]+1, size)


if __name__ == '__main__':
    main(*(map(int, sys.argv[1:])))
