#!/usr/bin/env python3

# fn = 'test2_input.txt'
fn = 'input.txt'


def parse(x):
    x = x.split(' ')
    if len(x) == 1:
        return (x[0], 0)
    return (x[0], int(x[1]))


def cyclecount(d):
    return 2*len([x for x in d if x[0] == 'addx']) + \
             len([x for x in d if x[0] == 'noop'])


def cycleslice(d, num):
    i = num//2
    cnt = cyclecount(d[0:i])
    while cnt < (num-1):
        i += (num - cnt)//2
        cnt = cyclecount(d[0:i])
    return d[0:i]


def strength(d):
    return 1 + sum([x[1] for x in d])


d = [parse(x) for x in open(fn).read().strip().split('\n')]

cycles = [20, 60, 100, 140, 180, 220]

print(sum([x*strength(cycleslice(d, x)) for x in cycles]))


def addpxl(crtp, img, lines):
    crtp += 1
    if crtp == 40:
        crtp = 0
        lines.append(list(img))
        img = []
    return (crtp, img)


x = 1
crtp = 0
img = []
lines = []
c = 0
for o in d:
    if o[0] == 'noop':
        c += 1
        if abs(crtp-x) <= 1:
            img.append('#')
        else:
            img.append('.')
        crtp, img = addpxl(crtp, img, lines)
    else:
        if abs(crtp-x) <= 1:
            img.append('#')
        else:
            img.append('.')
        crtp, img = addpxl(crtp, img, lines)
        if abs(crtp-x) <= 1:
            img.append('#')
        else:
            img.append('.')
        crtp, img = addpxl(crtp, img, lines)
        c += 2
        x += o[1]

for img in lines:
    print("".join(img))
