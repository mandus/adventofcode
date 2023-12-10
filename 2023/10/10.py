#!/usr/bin/env python3


def adjc(i: int, j: int) -> set:
    return [(x, y) for x in range(i-1, i+2) for y in range(j-1, j+2) if 1 <= sum([abs(x), abs(y)])]


fn = 't1.txt'
# fn = 'input.txt'
x = open(fn).read().strip().split('\n')
S = [e[0] for e in [[(j, i) for j, c in enumerate(r) if c == 'S'] for i, r in enumerate(x)] if e][0]


#
# I use (column, row) (aka (x, y)) - which mean look-up in data is x[row][column]


ps = {'|': ((0, -1), (0, 1)),
      '-': ((-1, 0), (1, 0)),
      'L': ((0, -1), (1, 0)),
      'J': ((0, -1), (-1, 0)),
      '7': ((-1, 0), (0, 1)),
      'F': ((1, 0), (0, 1))}


def comb(v, w):
    return (v[0]+w[0], v[1]+w[1])


def conn(pos, pipe):
    return [comb(pos, d) for d in ps[pipe]]


def look(pos, x):
    j, i = pos
    return x[i][j]


def shape(x):
    return len(x[0]), len(x)


def start(S, x):
    sh = shape(x)
    n = [(x[i][j], (j, i)) for j, i in adjc(*S) if x[i][j] in ps and 0 <= j < sh[1] and 0 <= i < sh[0]]
    map_n = [(c, conn(c[1], c[0])) for c in n]
    return [c for c in map_n if S in c[1]]


def nxt(c, p, x):
    # Given in c, and came from p - find next
    w = [v for v in c[1] if v != p][0]
    pipe = look(w, x)
    return ((pipe, w), conn(w, pipe))


def walk(S, x):
    # randomly select one of the start-neighbours as the first
    pipe = [S]
    go = start(S, x)[0]
    cur = go[0][1]
    pipe.append(cur)
    go = nxt(go, S, x)
    step = 2
    while S not in go[1]:
        step += 1
        prev = cur
        cur = go[0][1]
        pipe.append(cur)
        go = nxt(go, prev, x)
    pipe.append(go[0][1])
    return step+1, pipe

print(f'part1: {walk(S, x)[0] // 2}')

pipe = walk(S, x)[1]
c, r = shape(x)
for i in range(r):
    for j in range(c):
        s = x[i][j] if (j, i) in pipe else '.'
        print(s, end=''),
    print()

