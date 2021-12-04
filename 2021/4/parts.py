#!/usr/bin/env python3


# fn = 'test_input.txt'
fn = 'input.txt'


with open(fn, 'r') as f:
    lines = f.readlines()

part1 = ''
part2 = ''
nums = [int(i) for i in lines[0].split(',')]


def nullif(b, n):
    # If the value n is somewhere in board b, remove the value
    for i, r in enumerate(b):
        for j, v in enumerate(r):
            if v == n:
                b[i][j] = None


def null_row(b):
    for r in b:
        if not any(r):
            return True
    return False


def null_col(b):
    for j in range(len(b[0])):
        if not any([r[j] for r in b]):
            return True
    return False


def sum_board(b):
    return sum(sum(v for v in r if v) for r in b)


boards = []
board = []
for line in lines[2:]:
    inp = [int(i) for i in line.split()]
    if len(inp) == 0:
        boards.append(board)
        board = []
        continue
    board.append(inp)
boards.append(board)

board_sum = None
for num in nums:
    # remove from boards
    for b in boards:
        nullif(b, num)
    drop_boards = []
    for bidx, b in enumerate(boards):
        if null_row(b) or null_col(b):
            board_sum = sum_board(b)*num
            if not part1:
                part1 = board_sum
            drop_boards.append(bidx)
    for idx in reversed(sorted(drop_boards)):
        # remove winning board
        boards = boards[:idx] + boards[idx+1:]
# Store value for last winner
part2 = board_sum

print(f'part1: {part1}')
print(f'part2: {part2}')
