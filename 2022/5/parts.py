#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'

d = open(fn).read()
stacks, rearr = d.split('\n\n')
rearr = rearr.strip()


def linepart(line):
    if len(line) >= 4:
        return (line[:4], line[4:])
    else:
        return (line[:4], [])


def st(sd):
    s = {k: [] for k in sd[-1].strip().split('   ')}

    for line in sd[-2::-1]:
        process = True
        re = line
        key = "1"
        while process:
            fi, re = linepart(re)
            if "[" in fi:
                s[key].append(fi.strip())
            key = f'{int(key) + 1}'
            if not re:
                process = False
    return s


def p1(data, tasks):
    for task in tasks.split('\n'):
        tl = task.strip().split(' ')
        num = int(tl[1])
        fr = tl[3]
        to = tl[5]
        while num:
            item = data[fr].pop()
            data[to].append(item)
            num -= 1
    return data


def p2(data, tasks):
    for task in tasks.split('\n'):
        tl = task.strip().split(' ')
        num = int(tl[1])
        fr = tl[3]
        to = tl[5]
        moveitems = []
        while num:
            item = data[fr].pop()
            moveitems.insert(0, item)
            num -= 1
        data[to].extend(moveitems)
    return data


ds = st(stacks.split('\n'))
ds = p1(ds, rearr)

res = []
for i in range(1, len(ds)+1):
    res.append(ds[f'{i}'][-1][1])
print("".join(res))

ds = st(stacks.split('\n'))

ds = p2(ds, rearr)

res = []
for i in range(1, len(ds)+1):
    res.append(ds[f'{i}'][-1][1])
print("".join(res))
