#!/usr/bin/env python3


#  fn = 'test_input.txt'
fn = 'input.txt'


def transform(x):
    return [p.strip().split() for p in x.split('|')]


data = [transform(li.strip()) for li in open(fn).readlines()]
out = [p[1] for p in data]

# part 1 - count output words representing 1, 4, 7, or 8
# 1: len 2
# 4: len 4
# 7: len 3
# 8: len 7
lens = [2, 3, 4, 7]
print('part1:', sum(sum(1 for w in line if len(w) in lens) for line in out))


# part 2

# dab =      7 = [1, 0, 1, 0, 0, 1, 0] = [d,  ,  ,  ,  ,  ,  ]
# eafb =     4 = [0, 1, 1, 1, 0, 1, 0] = [ , e, a, f,  , b,  ]
# ab  =      1 = [0, 0, 1, 0, 0, 1, 0] = [ ,  , a,  ,  , b,  ]
# acedgfb =  8 = [1, 1, 1, 1, 1, 1, 1] = [d,  , a,  ,  , b,  ]
# cdfbe =    5 = [1, 1, 0, 1, 0, 1, 0] = [d, e,  , f,  , b, c]
# gcdfa =    2 = [1, 0, 1, 1, 1, 0, 1] = [d,  , a, f, g,  , c]
# fbcad=     3 = [1, 0, 1, 1, 0, 1, 1] = [d,  , a, f,  , b, c]

# - and then all letters are mapped to positions, and the digit can be read.
# -> but in this example we had all 5, 2, 3 which maps the remainig parts.
# It should be possible to use any combination of them
# And if they are not present, we need 6, 9 and probably 5...

inp = [p[0] for p in data]
inpwords = list(sorted(list(set(len(w) for w in line))) for line in inp)

def is_three(w, lm):
    assert lm[0][0]
    assert lm[2][0]
    assert lm[5][0]
    testchrs = list(set(lm[0] + lm[2] + lm[5]))
    return(len([c for c in w if c in testchrs]) == len(testchrs))


def is_five(w, lm):
    assert lm[0][0]
    assert lm[1][0]
    assert lm[3][0]
    testchrs = list(set(lm[0] + lm[1] + lm[3]))
    return(len([c for c in w if c in testchrs]) == len(testchrs))


def mapletters(l):
    lm = {d: None for d in range(7)}
    l = sorted(l, key=lambda x: len(x))
    for w in l:
        if len(w) == 2:
            lm[2] = [c for c in w]
            lm[5] = [c for c in w]
        elif len(w) == 3:
            lm[0] = [c for c in w if c not in lm[2]]
        elif len(w) == 4:
            lm[1] = [c for c in w if c not in lm[2] + lm[5]]
            lm[3] = [c for c in w if c not in lm[2] + lm[5]]
        elif len(w) == 5:
            if is_five(w, lm):
                lm[5] = [c for c in w if c in lm[5]]
                lm[2] = [c for c in lm[2] if c not in lm[5]]
                lm[6] = [c for c in w if c not in lm[0] + lm[1] + lm[3] + lm[5]]
                if lm[4]:
                    lm[4] = [c for c in lm[4] if c not in lm[6]]
            elif is_three(w, lm):
                lm[3] = [c for c in w if c in lm[3]]
                lm[1] = [c for c in lm[1] if c not in lm[3]]
                lm[6] = [c for c in w if c not in lm[0] + lm[2] + lm[3] + lm[5]]
                if lm[4]:
                    lm[4] = [c for c in lm[4] if c not in lm[6]]
            else:  # must be 2; only option of len 5 left
                lm[2] = [c for c in lm[2] if c in w]
                lm[5] = [c for c in lm[5] if c not in lm[2]]
                lm[3] = [c for c in lm[3] if c in w]
                lm[1] = [c for c in lm[1] if c not in lm[3]]
                if lm[6]:
                    lm[4] = [c for c in w if c not in lm[0] + lm[2] + lm[3] + lm[6]]
                else:
                    lm[4] = [c for c in w if c not in lm[0] + lm[2] + lm[3]]
                    lm[6] = [c for c in w if c not in lm[0] + lm[2] + lm[3]]
        if (not [c for _, c in lm.items() if not c]) and not([d for d, c in lm.items() if not len(c) == 1]):
            return {c[0]: p for p, c in lm.items()}
    raise Exception('Unable to map:', lm)


def mapword(w, lm):
    pat = [0 for _ in range(7)]
    for c in w:
        pat[lm[c]] = 1
    return pat


def mappat(pat):
    if pat == [0, 0, 1, 0, 0, 1, 0]: return 1
    if pat == [1, 0, 1, 1, 1, 0, 1]: return 2
    if pat == [1, 0, 1, 1, 0, 1, 1]: return 3
    if pat == [0, 1, 1, 1, 0, 1, 0]: return 4
    if pat == [1, 1, 0, 1, 0, 1, 1]: return 5
    if pat == [1, 1, 0, 1, 1, 1, 1]: return 6
    if pat == [1, 0, 1, 0, 0, 1, 0]: return 7
    if pat == [1, 1, 1, 1, 1, 1, 1]: return 8
    if pat == [1, 1, 1, 1, 0, 1, 1]: return 9
    if pat == [1, 1, 1, 0, 1, 1, 1]: return 0


def digtonum(dig):
    mul = 1
    num = 0
    for d in reversed(dig):
        try:
            num += d*mul
        except:
            raise
        mul *= 10
    return num


print('part2: ', sum(digtonum([mappat(mapword(w, mapletters(line))) for w in out]) for line, out in zip(inp, out)))
