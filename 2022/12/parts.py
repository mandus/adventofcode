#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'

# Strategy

# 0. At a position p, get reachable neighbours
# 1. If 'S' is reachable, return distance 1
# 2. Else, add p to a dict of seen nodes, and find distance from each neighbour n
# 3. return 1 + min(distances) (for equal lengths, it doesn't matter)
# NOTE: This isn't going to be tail recursive, so we may get deep stacks

# Although this simple strategy works for the test-case, it's too heavy for the real problem


def find(d, c):
    val = ord(c)
    row, st_li = [(idx, li) for idx, li in enumerate(d) if val in li][0]
    col = [idx for idx, v in enumerate(st_li) if v == val][0]
    return (row, col)

def findall(d, c):
    val = ord(c)
    return [(row, col) for (row, li) in enumerate(d) for (col, pos) in enumerate(li) if pos == val]


def look(d, t):
    """Look up tuple (row, col) in d"""
    return d[t[0]][t[1]]


def inside(d, p):
    """True if p is inside d"""
    rs = len(d)
    cs = len(d[0])
    return 0 <= p[0] < rs and 0 <= p[1] < cs


def add(x, y):
    return (x[0]+y[0], x[1]+y[1])


def neigh(d, p):
    dirs = ((1, 0), (0, 1), (-1, 0), (0, -1))
    return [x for x in [add(p, x) for x in dirs] if inside(d, x)]


def reach(d, p, c):
    """Return points in c that is at most one above value in p"""
    cur = max(ord('a'), look(d, p)) + 1
    return [pt for pt in c if ord('a') <= look(d, pt) <= cur]


def distance(d, p, end):
    visited = {}
    queue = [(p, 0)]
    maxcost = 2*len(d)*len(d[0])

    while queue:
        t, dist = queue.pop()
        cands = neigh(d, t)


        if t in visited:
            continue

        visited[t] = dist

        if end in cands and look(d, t) >= ord('y'):
            visited[end] = min(dist+1, visited.get(end, maxcost))
        else:
            for c in reach(d, t, cands):
                if c not in visited:
                    queue.insert(0, (c, dist+1))

    return visited.get(end, False)



def part1(d, st, ed):
    print('part 1 ', distance(d, st, ed))


def part2(d, ed):
    sts = findall(d, 'a') + [find(d, 'S')]
    print('part 2: ', min([x for x in [distance(d, st, ed) for st in sts] if x]))


if __name__ == '__main__':
    d = [[ord(p) for p in li.strip()] for li in open(fn).read().strip().split()]
    st = find(d, 'S')
    ed = find(d, 'E')
    part1(d, st, ed)
    part2(d, ed)
