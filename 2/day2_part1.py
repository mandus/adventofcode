#!/usr/bin/env python3

d = [int(d) for d in open('input.txt').readline().strip().split(',')]

testdata = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]


def restoredata(data):
    data[1] = 12
    data[2] = 2
    return data


def leftop(data, c):
    return data[data[c+1]]


def rightop(data, c):
    return data[data[c+2]]


def runit(data):
    pos = 0
    while True:
        opcode = data[pos]
        if opcode == 99:
            break
        left = leftop(data, pos)
        right = rightop(data, pos)
        if opcode == 1:
            val = left + right
        elif opcode == 2:
            val = left * right
        savepos = data[pos+3]
        data[savepos] = val
        pos += 4
    return data


d = restoredata(d)
d = runit(d)
print(d[0])
