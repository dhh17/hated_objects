#!/usr/bin/env python3

import sys

corrections = {}

def tsvparse(x):
    return x.rstrip().split('\t')

def csvparse(x):
    return x.rstrip().split(',')

def tsvdo(l):
    return '\t'.join(l)

HEADERS = ['Places polished', 'Polished', 'Old', 'Correct']

for filename in ['tea.txt', 'wine.txt']:
    with open(filename) as f:
        header = tsvparse(next(f))
        assert header[0] in HEADERS
        for l in f:
            row = tsvparse(l)
            if len(row) < 2:
                continue
            if len(row) == 2:
                a, b = row
                c = ''
            else:
                a, b, c = row
            if b == '' and c == '':
                continue
            if a in corrections:
                if corrections[a] != (b,c):
                    # print([a, corrections[a], b])
                    pass
            else:
                corrections[a] = (b,c)

for filename in ['old_to_modern.csv']:
    with open(filename) as f:
        header = csvparse(next(f))
        assert header[0] in HEADERS
        for l in f:
            row = csvparse(l)
            if len(row) < 2:
                continue
            if len(row) == 2:
                a, b = row
                c = ''
            else:
                a, b, c = row
            if b == '' and c == '':
                continue
            if a in corrections:
                if corrections[a] != (b,c):
                    # print([a, corrections[a], b])
                    pass
            else:
                corrections[a] = (b,c)

f = sys.stdin
header = tsvparse(next(f))
assert header[0] in HEADERS
print(tsvdo(header))
for l in sys.stdin:
    row = tsvparse(l)
    if len(row) == 3:
        a,b,c = row
    elif len(row) == 2:
        a,b,c = row[0],row[1],''
    else:
        a,b,c = row[0],'',''
    if a != '' and a in corrections:
        if b == '':
            b = corrections[a][0]
        if c == '':
            c = corrections[a][1]
    print(tsvdo([a,b,c]))
