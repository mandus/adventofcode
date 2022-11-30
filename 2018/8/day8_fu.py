#!/usr/bin/env python3
# Advent of Code 2018 - Åsmund Ødegård
# more functional variant - transistional

"""
Datastructure:
    node = ((#children, #metadata) [<node>...], [metadata])
    header = node[0]
    children = node[1]
    metadata = node[2]
"""

import sys


def read_input(fn):
    """read input"""
    with open(fn, 'r') as f:
        line = f.read().splitlines()
    return [int(i) for i in line[0].split()]


def createnode(children, mets, data):
    n = ((children, mets), [], [])
    for i in range(children):
        children = data.pop(0)
        mets = data.pop(0)
        n[1].append(createnode(children, mets, data))
    for i in range(n[0][1]):
        n[2].append(data.pop(0))
    return n


def checksum(n):
    return sum(checksum(c) for c in n[1]) + sum(n[2])


def nodesum(n):
    if n[0][0]:
        return sum([nodesum(n[1][idx-1]) for idx in n[2] if idx <= n[0][0]])
    else:
        return sum(n[2])


def main(fn):
    data = read_input(fn)

    children = data.pop(0)
    mets = data.pop(0)
    root = createnode(children, mets, data)

    # part 1
    print('checksum:', checksum(root))

    # part 2
    print('rootsum:', nodesum(root))


if __name__ == '__main__':
    main(sys.argv[1])
