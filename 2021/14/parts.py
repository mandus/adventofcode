#!/usr/bin/env python3

# fn = 'test_input.txt'
fn = 'input.txt'


def parserules(r: str) -> dict:
    return {k: v for k, v in [li.split(' -> ') for li in r.split('\n')]}


def tokenize(t: str) -> dict:
    tm = {}
    for i in range(len(t)-1):
        token = t[i:i+2]
        tm[token] = tm.get(token, 0) + 1
    return tm


def step(tm: dict, r: dict) -> dict:
    nxt = {}
    for token, count in tm.items():
        left = token[0] + r[token]
        right = r[token] + token[1]
        nxt[left] = nxt.get(left, 0) + count
        nxt[right] = nxt.get(right, 0) + count
    return nxt


def charfreq(tm: dict) -> dict:
    m = {}
    for token, count in tm.items():
        l, r = token[0], token[1]
        m[l] = m.get(l, 0) + count
    else:
        m[r] = m.get(r, 0) + 1
    return m


templ, rules = open(fn).read().strip().split('\n\n')

rules = parserules(rules)
tokens = tokenize(templ)

for i in range(10):
    tokens = step(tokens, rules)
charcounts = sorted(charfreq(tokens).values())
print('part 1', charcounts[-1] - charcounts[0])

for i in range(30):
    tokens = step(tokens, rules)
charcounts = sorted(charfreq(tokens).values())
print('part 2', charcounts[-1] - charcounts[0])
