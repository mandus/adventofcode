#!/usr/bin/env python3
# Advent of Code 2018 - Åsmund Ødegård

import sys


class node:
    def __init__(s, children, mets):
        s.header = (children, mets)
        s.children = []
        s.metadata = []

    def checksum(s):
        return sum(c.checksum() for c in s.children) + sum(s.metadata)

    def nodesum(s):
        if s.header[0]:
            return sum([s.children[idx-1].nodesum() for idx in s.metadata if idx <= s.header[0]])
        else:
            return sum(s.metadata)

    def __repr__(s):
        return "%s: [%s %s]" % (s.header, len(s.children), len(s.metadata))


def read_input(fn):
    """read input"""
    with open(fn, 'r') as f:
        line = f.read().splitlines()
    return [int(i) for i in line[0].split()]


def createnode(children, mets, data):
    n = node(children, mets)
    for i in range(children):
        children = data.pop(0)
        mets = data.pop(0)
        n.children.append(createnode(children, mets, data))
    for i in range(n.header[1]):
        n.metadata.append(data.pop(0))
    return n


def main(fn):
    data = read_input(fn)

    children = data.pop(0)
    mets = data.pop(0)
    root = createnode(children, mets, data)

    # part 1
    print('checksum:', root.checksum())

    # part 2
    print('rootsum:', root.nodesum())


if __name__ == '__main__':
    main(sys.argv[1])
