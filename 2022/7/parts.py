#!/usr/bin/env python3

# Datastructure:
# Current path:
# p = ['/','a',..]
#
# Filestystem
# fs: {'/': {'a': {'e': {}},
#            'b.txt': 148..,
#            'c.dat': 1234..,
#            'd': {}})}
#
#
# dsize: {'/': <totalsize>
#         '/a': <totalsize>
#


def cd(fs, p, path):
    if path == '..':
        p.pop()
    else:
        p.append(path)
    return (fs, p)


def getpath(cur, revp):
    d = revp.pop()
    if revp:
        return getpath(cur[d], revp)
    else:
        return cur[d]


def ls(fs, p, content):
    c = {}
    for part in content:
        data, name = part.split(' ')
        if data == 'dir':
            c[name] = {}
        else:
            c[name] = int(data)
    h = getpath(fs, p[::-1])
    h.update(c)
    return (fs, p)


def exe(d, fs, p):
    for cmd in d:
        if cmd.startswith('cd'):
            (fs, p) = cd(fs, p, cmd[len('cd '):])
        if cmd.startswith('ls'):
            (fs, p) = ls(fs, p, cmd.split('\n')[1:])
    return fs


def dirsize(fs, p, dsize):
    size = 0
    for n, data in fs.items():
        if isinstance(data, dict):
            sz, _ = dirsize(data, p + [n], dsize)
            size += sz
        else:
            size += data
    dsize[p[0]+"/".join(p[1:])] = size
    return (size, dsize)


def findminrm(dsize, tot, need):
    free = tot - dsize['/']
    return min([sz for p, sz in dsize.items() if free + sz > need])


if __name__ == '__main__':
    # fn = 'test_input.txt'
    fn = 'input.txt'
    d = [x.strip() for x in open(fn).read().split('$') if x.strip()]
    fs = exe(d, {'/': {}}, [])
    (_, dsize) = dirsize(fs['/'], ['/'], {})
    print('part1: ', sum([s for p, s in dsize.items() if s <= 100000]))

    tot = 70000000
    need = 30000000
    print('part2: ', findminrm(dsize, tot, need))
