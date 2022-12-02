#!/usr/bin/env python3

#  fn = 'test_input.txt'
fn = 'input.txt'

# Score:
ts = {'X': 1, 'Y': 2, 'Z': 3}

#  X  Y  Z
res = {'A X': 3,
       'A Y': 6,
       'A Z': 0,
       'B X': 0,
       'B Y': 3,
       'B Z': 6,
       'C X': 6,
       'C Y': 0,
       'C Z': 3}

d = open(fn).read().strip().split('\n')

print('part1 ',
      sum([res[x] for x in d]) +
      sum([ts[x.split(' ')[1]] for x in d]))


# X = win
# Y = draw
# Z = loose
ts = {'R': 1, 'P': 2, 'S': 3}
res = {'X': 0, 'Y': 3, 'Z': 6}

m = {}
m['Z'] = {'A': 'P', 'B': 'S', 'C': 'R'}  # Win
m['Y'] = {'A': 'R', 'B': 'P', 'C': 'S'}  # Draw
m['X'] = {'A': 'S', 'B': 'R', 'C': 'P'}  # Loose
print('part2 ',
      sum([res[x.split()[1]] for x in d]) +
      sum([ts[m[x.split()[1]][x.split()[0]]] for x in d]))
