# Some maybe useful python snippets


# fn = 'test_input.txt'
fn = 'input.txt'

# just split by line
d = open(fn).read().strip().split('\n')

# split by line, and split every line
d = [x.split() for x in open(fn).read().strip().split('\n')]


# split a list in half, assuming it has even elements
# also converting each part to set if flag is Tru
def half(line, retset=False):
    h = len(line)//2
    if retset:
        return (set(line[:h]), set(line[h:]))
    return (line[:h], line[h:])


# convert a-z into 1-26 and A-Z into 27-52
def chrnum(c):
    # ascii values for a couple of letters
    a = ord('a')
    A = ord('A')

    cv = ord(c)
    if cv < a:
        return cv - A + 27
    return cv - a + 1
