#!/usr/bin/env python3

# AoC 2019 - day 4 - Åsmund Ødegård


start = 356261
end = 846303


def locks(s, e):
    while s <= e:
        ls = str(s)
        ok = True
        idx = 0

        twoadjequal = False
        while ok:

            if ls[idx] > ls[idx+1]:
                ok = False
                continue
            idx += 1

            twoadjequal = twoadjequal or ls[idx-1] == ls[idx]

            if idx > 4:
                ok = twoadjequal
                break

        if ok:
            yield s
        s += 1


print('part1:', len([a for a in locks(start, end)]))


def valid_counts(l):
    ls = str(l)
    lm = {}
    for idx in range(6):
        char = ls[idx]
        lm[char] = lm.get(char, 0) + 1
    return len([v for k, v in lm.items() if v == 2]) >= 1


print('part2:', len([a for a in locks(start, end) if valid_counts(a)]))
