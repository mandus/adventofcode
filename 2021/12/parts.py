#!/usr/bin/env python3

def get_data(fn: str) -> list:
    return [li.strip().split('-') for li in open(fn).readlines()]


def is_lower(cave: str) -> bool:
    return cave.lower() == cave


def no_lower_dupe(path: list) -> bool:
    l_c = list(filter(is_lower, path))
    return len(set(l_c)) == len(l_c)


def map_data(data: list) -> dict:
    cs_map = {}
    for start, end in data:
        cs_map[start] = cs_map.get(start, []) + [end]
        cs_map[end] = cs_map.get(end, []) + [start]
    return cs_map


def visitcave(cavemap: dict, cave: str, paths: list, path: list, allow_lower_once: bool = False) -> None:
    # TODO: paths is state, so at the moment not really functional. Need to work on that
    path.append(cave)
    if cave == 'end':
        paths.append(path)
        return paths
    for nxt in cavemap[cave]:
        if nxt == 'start':
            # never visit 'start twice
            continue
        if nxt in path and is_lower(nxt):
            if allow_lower_once and no_lower_dupe(path):
                paths = visitcave(cavemap, nxt, paths, [c for c in path], allow_lower_once)
            continue
        paths = visitcave(cavemap, nxt, paths, [c for c in path], allow_lower_once)
    return paths


def part1(cavemap: dict) -> None:
    print('part1: ', len(visitcave(cavemap, 'start', [], [])))


def part2(cavemap: dict) -> None:
    print('part2: ', len(visitcave(cavemap, 'start', [], [], True)))


def main():
    fn = 'input.txt'
    data = get_data(fn)
    connections = map_data(data)
    part1(connections)
    part2(connections)


if __name__ == '__main__':
    main()


#  TESTS

def test1_part1() -> None:
    cavemap = map_data(get_data('test_input.txt'))
    assert len(visitcave(cavemap, 'start', [], [])) == 10


def test2_part1() -> None:
    cavemap = map_data(get_data('test_input1.txt'))
    assert len(visitcave(cavemap, 'start', [], [])) == 19


def test3_part1() -> None:
    cavemap = map_data(get_data('test_input2.txt'))
    assert len(visitcave(cavemap, 'start', [], [])) == 226


def test1_part2() -> None:
    cavemap = map_data(get_data('test_input.txt'))
    assert len(visitcave(cavemap, 'start', [], [], allow_lower_once=True)) == 36


def test2_part2() -> None:
    cavemap = map_data(get_data('test_input1.txt'))
    assert len(visitcave(cavemap, 'start', [], [], allow_lower_once=True)) == 103


def test3_part2() -> None:
    cavemap = map_data(get_data('test_input2.txt'))
    assert len(visitcave(cavemap, 'start', [], [], allow_lower_once=True)) == 3509
