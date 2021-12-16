#!/usr/bin/env python3

from functools import reduce


def btod(b):
    return sum(v*(2**i) for i, v in enumerate(reversed(b)))


def htob(h):
    hm = {
            '0': [0, 0, 0, 0],
            '1': [0, 0, 0, 1],
            '2': [0, 0, 1, 0],
            '3': [0, 0, 1, 1],
            '4': [0, 1, 0, 0],
            '5': [0, 1, 0, 1],
            '6': [0, 1, 1, 0],
            '7': [0, 1, 1, 1],
            '8': [1, 0, 0, 0],
            '9': [1, 0, 0, 1],
            'A': [1, 0, 1, 0],
            'B': [1, 0, 1, 1],
            'C': [1, 1, 0, 0],
            'D': [1, 1, 0, 1],
            'E': [1, 1, 1, 0],
            'F': [1, 1, 1, 1]}
    return hm[h]


def parse_lit(d: list) -> tuple:
    lit_digs = []
    while True:
        lit_digs.extend(d[1:5])
        if d[0] == 0:
            d = d[5:]
            break
        d = d[5:]
    return d, btod(lit_digs)


def parse(d: list) -> tuple:
    vrs = btod(d[0:3])
    tid = btod(d[3:6])
    d = d[6:]

    if tid == 4:
        d, lit = parse_lit(d)
        p = {'v': vrs, 't': tid, 'lit': lit}
        return p, d
    else:
        lentype = d[0]
        subops = []
        if lentype == 0:
            size = btod(d[1:16])
            d = d[16:]
            tmpd = d[0:size]
            while True:
                ops, tmpd = parse(tmpd)
                subops.append(ops)
                if not tmpd:
                    break
            d = d[size:]
            p = {'v': vrs, 't': tid, 'sub': subops}
            return p, d
        elif lentype == 1:
            count = btod(d[1:12])
            subops = []
            d = d[12:]
            while True:
                ops, d = parse(d)
                subops.append(ops)
                if len(subops) >= count:
                    break
            p = {'v': vrs, 't': tid, 'sub': subops}
            return p, d


def addversions(pcks: list) -> int:
    vs = 0
    for p in pcks:
        vs += p['v']
        if p['t'] != 4:
            vs += addversions(p['sub'])
    return vs


def valuepck(pck: dict) -> int:
    # base-case is literal pck which value is itself
    if pck['t'] == 4:
        return pck['lit']

    subvals = (valuepck(p) for p in pck['sub'])
    tid = pck['t']

    if tid == 0:
        return sum(subvals)
    if tid == 1:
        return reduce(lambda x, y: x*y, subvals)
    if tid == 2:
        return min(subvals)
    if tid == 3:
        return max(subvals)

    lsv = list(subvals)
    if tid == 5:
        return lsv[0] > lsv[1]
    if tid == 6:
        return lsv[0] < lsv[1]
    if tid == 7:
        return lsv[0] == lsv[1]


def transform(datastr: str) -> list:
    return [int(d) for sub in [htob(c) for c in datastr] for d in sub]


def main():
    fn = 'input.txt'
    data = transform(open(fn).readline().strip())
    pcks, _ = parse(data)
    print('part1 ', addversions([pcks]))
    print('part2 ', valuepck(pcks))


if __name__ == '__main__':
    main()


#
#  tests
#


def test_mini():
    d = transform('D2FE28')
    pcks, _ = parse(d)
    assert pcks['lit'] == 2021


def test_subpcks():
    d = transform('38006F45291200')
    pcks, _ = parse(d)
    assert pcks['v'] == 1
    assert pcks['t'] == 6
    assert len(pcks['sub']) == 2

    d = transform('EE00D40C823060')
    pcks, _ = parse(d)
    print(pcks)
    assert pcks['v'] == 7
    assert pcks['t'] == 3
    assert len(pcks['sub']) == 3


def test_add_versions():
    d = transform('8A004A801A8002F478')
    pcks, _ = parse(d)
    assert addversions([pcks]) == 16

    d = transform('620080001611562C8802118E34')
    pcks, _ = parse(d)
    assert addversions([pcks]) == 12

    d = transform('C0015000016115A2E0802F182340')
    pcks, _ = parse(d)
    assert addversions([pcks]) == 23

    d = transform('A0016C880162017C3686B18A3D4780')
    pcks, _ = parse(d)
    assert addversions([pcks]) == 31


def test_sum_pcks():
    d = transform('C200B40A82')
    pcks, _ = parse(d)
    assert valuepck(pcks) == 3

    d = transform('04005AC33890')
    pcks, _ = parse(d)
    assert valuepck(pcks) == 54

    d = transform('880086C3E88112')
    pcks, _ = parse(d)
    assert valuepck(pcks) == 7

    d = transform('CE00C43D881120')
    pcks, _ = parse(d)
    assert valuepck(pcks) == 9

    d = transform('D8005AC2A8F0')
    pcks, _ = parse(d)
    assert valuepck(pcks) == 1

    d = transform('F600BC2D8F')
    pcks, _ = parse(d)
    assert valuepck(pcks) == 0

    d = transform('9C005AC2F8F0')
    pcks, _ = parse(d)
    assert valuepck(pcks) == 0

    d = transform('9C0141080250320F1802104A08')
    pcks, _ = parse(d)
    assert valuepck(pcks) == 1
