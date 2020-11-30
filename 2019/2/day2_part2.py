#!/usr/bin/env python3

from copy import copy

data = [int(d) for d in open('input.txt').readline().strip().split(',')]

testdata = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]

goal = 19690720


def restoredata(data, noun=12, verb=2):
    data[1] = noun
    data[2] = verb
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


d = [0]
done = False
for noun in range(100):
    if done:
        break
    for verb in range(100):
        d = restoredata(copy(data), noun, verb)
        d = runit(d)
        if d[0] == goal:
            print("***********")
            print('  ', 100*noun+verb)
            print("***********")
            done = True
            break
