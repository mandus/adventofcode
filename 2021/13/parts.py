#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'


def dotcoors(d: list) -> set:
    return set(tuple(map(int, c.split(','))) for c in d.split())


def foldcoord(fl: list) -> tuple:
    if fl[0] == 'x':
        return (int(fl[1]), 0)
    return (0, int(fl[1]))


def parsefolds(f: list) -> list:
    tl = len('fold along ')
    return [foldcoord(t[tl:].split('=')) for t in f.split('\n')]


def fold(d:  set, f: tuple) -> set:
    fx, fy = f
    if fx != 0:
        return set([(fx-(dot[0]-fx), dot[1]) if dot[0] > fx else dot for dot in d])
    return set([(dot[0], fy-(dot[1]-fy)) if dot[1] > fy else dot for dot in d])


def printdots(d: list) -> None:
    maxx = max(x[0] for x in d)
    maxy = max(x[1] for x in d)

    pic = []
    for j in range(maxy+1):
        chrs = []
        for i in range(maxx+1):
            if (i, j) in d:
                chrs.append('#')
            else:
                chrs.append(' ')
        pic.append(''.join(chrs))
    [print(p) for p in pic]


dots, folds = open(fn).read().strip().split('\n\n')
dots = dotcoors(dots)
folds = parsefolds(folds)

print('part1', len(fold(dots, folds[0])))


for f in folds:
    dots = fold(dots, f)


print('part2')
printdots(dots)
