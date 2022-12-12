#!/usr/bin/env python3

from operator import mul, add
from functools import reduce

# fn = 'test_input.txt'
fn = 'input.txt'


def monkey(x):
    m = x[len('Monkey '):-1]
    return m


def business(x):
    m = {'items': [], 'op': lambda x: x, 'test': lambda x: x}
    m['items'] = [int(i.strip()) for i in x[0].strip()[len('Starting items: '):].split(',')]

    op, val = x[1].strip()[len('Operation: new = old '):].split(' ')
    opr = None
    val = val.strip()
    if op.strip() == '+':
        opr = add
    elif op.strip() == '*':
        opr = mul
    else:
        raise Exception('unknown operator')
    if val == 'old':
        op = lambda x: opr(x, x)
    else:
        val = int(val)
        op = lambda x: opr(x, val)

    mod = int(x[2].split(' ')[-1])
    tr = x[3].split(' ')[-1].strip()
    fal = x[4].split(' ')[-1].strip()
    tst = lambda x: fal if x % mod else tr

    m['op'] = op
    m['test'] = tst
    return (m, mod)


def round(ms, d, relief, mod=1):
    insp = []
    for m in ms:
        insp.append(len(d[m]['items']))
        handle = [h for h in d[m]['items']]
        d[m]['items'] = []
        for item in handle:
            new = d[m]['op'](item)//relief
            new = new % mod if mod > 1 else new
            throw = d[m]['test'](new)
            d[throw]['items'].append(new)

    return insp


def part1(ms, d, mod=1):
    tinsp = [0 for _ in ms]
    for _ in range(20):
        insp = round(ms, d, 3, mod)
        tinsp = [tinsp[i] + v for i, v in enumerate(insp)]
    tinsp.sort()
    print('part1 :', tinsp[-2]*tinsp[-1])


def part2(ms, d, mod=1, rnds=20):
    tinsp = [0 for _ in ms]
    # this doesn't work of course!
    for _ in range(rnds):
        insp = round(ms, d, 1, mod)
        tinsp = [tinsp[i] + v for i, v in enumerate(insp)]
    tinsp.sort()
    print('part2 :', tinsp[-2]*tinsp[-1])


if __name__ == '__main__':
    d_mod = {monkey(x.split('\n')[0]): business(x.split('\n')[1:]) for x in open(fn).read().split('\n\n')}
    monkeys = d_mod.keys()
    d = {m: b[0] for m, b in d_mod.items()}
    glob_mod = reduce(mul, [b[1] for m, b in d_mod.items()])
    part1(monkeys, d, glob_mod)

    d_mod = {monkey(x.split('\n')[0]): business(x.split('\n')[1:]) for x in open(fn).read().split('\n\n')}
    d = {m: b[0] for m, b in d_mod.items()}
    part2(monkeys, d, glob_mod, rnds=10000)
