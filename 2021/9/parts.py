#!/usr/bin/env python3

fn = 'test_input.txt'
#  fn = 'input.txt'

data = [[int(c) for c in li.strip()] for li in open(fn).readlines()]
rows = len(data)
cols = len(data[0])

def lowpoint(d, i, j, ro, co):
    p = d[i][j]
    adjs= [[i,j-1], [i-1,j], [i+1,j], [i,j+1]]
    return all(d[a[0]][a[1]]-d[i][j] > 0 for a in adjs if 0 <= a[0] <= ro-1 and 0 <= a[1] <= co-1)
    

print('part1: ', sum(data[i][j]+1 for i in range(rows) for j in range(cols) if lowpoint(data, i, j, rows, cols)))