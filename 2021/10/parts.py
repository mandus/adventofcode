#!/usr/bin/env python3


fn = 'test_input.txt'
#  fn = 'input.txt'


def checkline(l):
    stack = []
    opening = '({[<'
    closing = ')}]>'
    chk_map = {
            '(': ')',
            '[': ']',
            '{': '}',
            '<': '>'}

    for c in l:
        if c in opening:
            stack.append(c)
            print(f'st {stack}')
        elif c in closing and chk_map[c] == stack[-1]:
            stack = stack[:-1]
        else:
            return False
    return True


data = [li.strip() for li in open(fn).readlines()]
print(data)
non_corrupt = [line for line in data if checkline(line)]
