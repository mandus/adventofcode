#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'

d = [x.split() for x in open(fn).read().strip().split('\n')]

# Score matrix
# rows: elf, cols: me
# 0 = R, 1 = P, 2 = S
sc = [[4, 8, 3], [1, 5, 9], [7, 2, 6]]

# Part 1:
map_elf = {'A': 0, 'B': 1, 'C': 2}
map_me = {'X': 0, 'Y': 1, 'Z': 2}
print('part1 ', sum([sc[map_elf[x]][map_me[y]] for (x, y) in d]))


# part 2:
def map_result(elf, me):
    m = {'X': {'A': 2, 'B': 0, 'C': 1},
         'Y': {'A': 0, 'B': 1, 'C': 2},
         'Z': {'A': 1, 'B': 2, 'C': 0}}
    return m[me][elf]


print('part2 ', sum([sc[map_elf[x]][map_result(x, y)] for (x, y) in d]))
