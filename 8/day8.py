#!/usr/bin/env python3

import operator


data = open("input.txt").read().strip()
# pixels 25 x 6
w = 25
h = 6

# data = "123456789012"
# data = "0222112222120000"
#
# w = 2
# h = 2
layer_size = w * h


def layer(d, i, l):
    """
    d: data
    i: layer number
    l: size of layer
    """
    return d[l*i:l*(i+1)]


def num_layers(d, l):
    """
    d: data
    l: size of layer
    """
    return int(len(d)/l)


def count_chars(d, l, char):
    """
    d: data
    l: size of layer
    char: character to search for
    """
    n_l = num_layers(d, l)
    chars_in_layer = {}
    for i in range(n_l):
        chars = [c for c in layer(d, i, l) if c == char]
        chars_in_layer[i] = len(chars)
    return chars_in_layer


# min_zeros = sorted(chars_in_layer.items(), key=operator.itemgetter(1))[0]
min_zeros_layer = sorted(count_chars(data, layer_size, "0").items(), key=operator.itemgetter(1))[0][0]
ones = count_chars(data, layer_size, "1")
twos = count_chars(data, layer_size, "2")

# part 1
print("part1")
print(min_zeros_layer)
print(ones[min_zeros_layer] * twos[min_zeros_layer])


# part 2
# 0: black, 1: white, 2: transparent

# find first non-transparent in each cell

img = list(layer(data, 0, layer_size))

for i in range(1, num_layers(data, layer_size)):
    update = layer(data, i, layer_size)
    img = [(lambda x, y: y if x == "2" else x)(*p) for p in zip(img, update)]


def img_row(d, r, width):
    return d[width*r:width*(r+1)]


def print_pix(p):
    return "X" if p == "1" else " "


print("\npart2\n")
num_rows = int(len(img)/w)
for i in range(num_rows):
    print("".join([print_pix(pixl) for pixl in img_row(img, i, w)]))
