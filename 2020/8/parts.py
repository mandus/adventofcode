#!/usr/bin/env python3

# fn = 'input_test.txt'
fn = 'input.txt'
debug = False

ops = {
        'nop': lambda pos, reg, val: (pos+1, reg),
        'jmp': lambda pos, reg, val: (pos+val, reg),
        'acc': lambda pos, reg, val: (pos+1, reg+val),
        }


def fixinstr(pos, op, fix, fixes):
    if not fix:
        if debug:
            print("fix disabled")
        return op, fix

    if op == 'acc':
        return op, fix

    if pos in fixes:
        # attempted before, not trying again
        return op, fix
    else:
        fixes[pos] = True

    fix = {'jmp': 'nop', 'nop': 'jmp'}[op]
    if debug:
        print(f'change {op} to {fix} in pos {pos}')
    return fix, False


def runops(instr, fix, fixes={}):
    mark = {}
    accu = 0
    pos = 0
    end = False

    while not end and pos not in mark:
        mark[pos] = mark.get(pos, 0) + 1
        op, data = instr[pos]
        data = int(data)

        op, fix = fixinstr(pos, op, fix, fixes)

        if debug:
            print(f"do op: {op}: {data} ({accu}) [{fix}]")

        pos, accu = ops[op](pos, accu, data)

        if debug:
            print("marks:", mark)
            print("fixes:", fixes)

        if pos >= len(instr):
            end = True
    return accu, end


def part1(instr):
    accu, end = runops(instr, False)
    print(f'part1: {accu}')


def part2(instr):
    end = False
    applyfix = {}
    while not end:
        accu, end = runops(instr, True, applyfix)
    print(f'part2: {accu}')


def fix(p,o,f,F):
    if any([not F, p in f or o == 'acc']): return o,F
    f[p]=1
    return {'jmp':'nop','nop':'jmp'}[o],0

def run(i,f={}):
    os = lambda op,p,r,v: {'nop':(p+1,r),'jmp':(p+v,r),'acc':(p+1,r+v)}[op]
    p,m,a,F=0,len(i),0,not(not f)
    while 1:
        i[p][2]=a; op,d,a=i[p]; op,F=fix(p,op,f,F); p,a=os(op,p,a,d)
        if not (p<m and i[p][2]==None): break
    return a,p<m

def p2(i):
    e,f=1,{-1:0}
    while e: a,e=run([x[:] for x in i],f)
    print(f'p2: {a}')

def s(fn):
    i=[[o,int(d),None] for (o,d) in [s.split() for s in open(fn).read().strip().split('\n')]]
    print(f'p1: {run([a[:] for a in i])[0]}') or p2(i)

if __name__ == '__main__':

    with open(fn) as f:
        instr = [s.strip().split() for s in f.readlines()]


    part1(instr)
    part2(instr)

    s(fn)
