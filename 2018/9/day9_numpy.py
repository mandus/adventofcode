#!/usr/bin/env python3
# Advent of Code 2018 - Åsmund Ødegård

import sys
import numpy as np


def read_line(fn):
    with open(fn, 'r') as f:
        line = f.readline().split()
    players = int(line[0])
    lastmarble = int(line[6])
    return (players, lastmarble)


def main(fn, mul=1):
    players, lastmarble = read_line(fn)
    lastmarble = lastmarble*int(mul)

    marbles = np.array([0])
    cur_marble_idx = 0
    next_marble = 1
    cur_player = 1
    scores = {}

    while next_marble <= lastmarble:

        if cur_marble_idx != 0 and next_marble % 23 == 0:
            # pop into score
            pop_idx = cur_marble_idx - 7
            if pop_idx < 0:
                pop_idx = len(marbles) + pop_idx
            pop_value = marbles[pop_idx]
            scores[cur_player] = scores.get(cur_player, 0) + next_marble + pop_value
            marbles = np.delete(marbles, [pop_idx])
            cur_marble_idx = pop_idx
        else:
            # insert
            next_idx = cur_marble_idx + 2
            if next_idx > len(marbles):
                next_idx = 1
            marbles = np.concatenate((marbles[:next_idx], np.array([next_marble]), marbles[next_idx:]))
            cur_marble_idx = next_idx

        cur_player += 1 % players
        if cur_player > players:
            cur_player = 1
        next_marble += 1

    max_score = max([v for k, v in scores.items()])
    print([(k, v) for k, v in scores.items() if v == max_score])


if __name__ == '__main__':
    main(*sys.argv[1:])
