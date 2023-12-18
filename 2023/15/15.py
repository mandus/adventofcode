#!/usr/bin/env python3

# fn = 't1.txt'
# fn = 't2.txt'
fn = 'input.txt'


def hash(s: str) -> int:
    asc = [ord(c) for c in s]
    v = 0
    for i in asc:
        v += i
        v *= 17
        v = v % 256
    return v


def labop(s):
    if '=' in s:
        lab, f = s.split('=')
        key, f = hash(lab), int(f)
        op = 'a'
    elif '-' in s:
        lab = s[:-1]
        key, f = hash(lab), None
        op = 'r'
    return lab, key, op, f


def upd_box(b: tuple, lab: str, op: str, f: int) -> tuple:
    # in each box, keep a list of lenses for ordering (lst)
    # and a dict of the lenses (m)
    if b:
        m, lst = b
    else:
        m, lst = {}, []
    if op == 'r' and lab in m:
        del m[lab]
        lst.remove(lab)
    if op == 'a':
        m[lab] = f
        if lab not in lst:
            lst.append(lab)
    return (m, lst)


def boxing(d: list) -> dict:
    # hash labels for keys in b
    b = {}
    for le in d:
        lab, key, op, f = labop(le)
        b[key] = upd_box(b.get(key, None), lab, op, f)
    return b


def b_pwr(no: int, b: tuple) -> int:
    # add for each lens, multiply
    # box number + 1; lens slot-number (1-index); focal length
    m, lst = b
    if not lst:
        return 0
    return sum([(no+1)*(idx+1)*m[le] for idx, le in enumerate(lst)])


def pwr(b: dict) -> int:
    # add all together
    return sum([b_pwr(key, box) for key, box in b.items()])


d = open(fn).read().strip().split(',')
print(f'part1: {sum([hash(s) for s in d])}')
print(f'part2: {pwr(boxing(d))}')
