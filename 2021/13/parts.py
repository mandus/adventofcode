#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'

dots, folds = open(fn).read().strip().split('\n\n')


def dotcoors(d: list) -> list:
    return [tuple(map(int, c.split(','))) for c in d.split()]


def foldcoord(fl: list) -> tuple:
    if fl[0] == 'x':
        return (int(fl[1]), 0)
    return (0, int(fl[1]))


def parsefolds(f: list) -> list:
    tl = len('fold along ')
    return [foldcoord(t[tl:].split('=')) for t in f.split('\n')]


def fold(d: list, f: tuple) -> list:
    fx, fy = f
    if fx != 0:
        return list(set([(fx-(dot[0]-fx), dot[1]) if dot[0] > fx else dot for dot in d]))
    return list(set([(dot[0], fy-(dot[1]-fy)) if dot[1] > fy else dot for dot in d]))


print('part1', len(fold(dotcoors(dots), parsefolds(folds)[0])))


dots = dotcoors(dots)
folds = parsefolds(folds)

for f in folds:
    dots = fold(dots, f)


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


print('part2')
printdots(dots)
