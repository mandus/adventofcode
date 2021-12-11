#!/usr/bin/env python3

#  fn = 'test_input.txt'
fn = 'input.txt'

data = [list(map(int, line.strip())) for line in open(fn).readlines()]
rows = len(data)
cols = len(data[0])


def step(d, r, c):
    for i in range(r):
        for j in range(c):
            d[i][j] += 1
    return d


def flashnow(d, r, c):
    return [(i, j) for i in range(r) for j in range(c) if d[i][j] == 10]


def stepnb(d, r, c, idx):
    nb = [(idx[0]+i, idx[1]+j) for i in range(-1, 2) for j in range(-1, 2)
          if not (i == 0 and j == 0) and 0 <= idx[0]+i <= r-1 and 0 <= idx[1]+j <= c-1]
    newflash = []
    for p in nb:
        pi, pj = p[0], p[1]
        d[pi][pj] += 1
        if d[pi][pj] == 10:
            newflash.append((pi, pj))
    return d, newflash


def flash(d, r, c):
    hls = flashnow(d, r, c)
    while hls:
        idx = hls[0]
        hls = hls[1:]
        d, extra = stepnb(d, r, c, idx)
        hls.extend(extra)
    return d


def resetflash(d, r, c):
    cnt = 0
    for i in range(r):
        for j in range(c):
            if d[i][j] > 9:
                cnt += 1
                d[i][j] = 0
    return d, cnt


def oneround(d, r, c):
    d = flash(step(d, r, c), r, c)
    return resetflash(d, r, c)


flashcount = 0
stepcnt = 0
superflash = False
while not superflash:
    data, cnt = oneround(data, rows, cols)
    flashcount += cnt
    stepcnt += 1

    if stepcnt == 100:
        print('part 1: ', flashcount)

    superflash = cnt == rows*cols

print('part2: ', stepcnt)
