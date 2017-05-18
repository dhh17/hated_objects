#!/usr/bin/env python3

import sys

corrections = {}

def tsvparse(x):
    return x.rstrip().split('\t')

def tsvdo(l):
    return '\t'.join(l)

for filename in ['coffee.txt', 'tea.txt', 'wine.txt']:
    with open(filename) as f:
        header = tsvparse(next(f))
        assert header[0] == 'Raw'
        for l in f:
            row = tsvparse(l)
            if len(row) < 2:
                continue
            a, b = row
            if b == '':
                continue
            if a in corrections:
                if corrections[a] != b:
                    # print([a, corrections[a], b])
                    pass
            else:
                corrections[a] = b

f = sys.stdin
header = tsvparse(next(f))
assert header[0] == 'Raw'
print(tsvdo(header))
for l in sys.stdin:
    row = tsvparse(l)
    if len(row) == 2:
        a,b = row
    else:
        a,b = row[0],''
    if a != '' and b == '':
        if a in corrections:
            b = corrections[a]
    print(tsvdo([a,b]))
