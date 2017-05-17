#!/usr/bin/env python3

import collections
import csv
import json
import re

Y1 = 1700
Y2 = 1800
YEARS = 10
TOP = 0.05

RANGES = list(range(Y1, Y2-YEARS+1))

def fix_estcid(x):
    assert x[0] in 'NPRTW', x
    return x[0] + re.sub(r'^0+', '', x[1:])

class Book:
    def __init__(self, r):
        self.estcid = r['estcid']
        self.nchar = r['nchar']
        self.npara = r['npara']
        self.npage = r['npage']
        self.year = r['year']
        if self.year is not None:
            if self.year < Y1 or self.year >= Y2:
                self.year = None
        self.hit_word = collections.Counter()
        self.hit_para = collections.Counter()


class Correlate:
    def __init__(self):
        self.cats = collections.defaultdict(list)
        self.hits_by_year_range = {}
        self.results = []

    def book_file(self, filename):
        print(filename)
        self.bookmap = {}
        self.booklist = []
        with open(filename) as f:
            results = json.load(f)
            for r in results:
                book = Book(r)
                self.booklist.append(book)
                self.bookmap[book.estcid] = book

    def hitfile(self, cat, word, filename):
        print(filename)
        key = (cat, word)
        self.cats[cat].append(word)
        with open(filename, newline='') as f:
            reader = csv.reader(f)
            header = next(reader)
            headermap = { h:i for i,h in enumerate(header) }
            for row in reader:
                estcid = fix_estcid(row[headermap['ESTCID']])
                hits = int(row[headermap['score']])
                assert hits > 0
                book = self.bookmap[estcid]
                book.hit_word[key] += hits
                book.hit_para[key] += 1

    def calc_total(self):
        by_year1 = collections.Counter()
        for book in self.booklist:
            if book.year is None:
                continue
            if book.year < Y1 or book.year >= Y2:
                continue
            by_year1[book.year] += 1
        self.total_by_year_range = collections.Counter()
        for y1 in RANGES:
            for year in range(y1, y1+YEARS):
                self.total_by_year_range[y1] += by_year1[year]

    def calc(self, cat, word):
        key = (cat, word)
        total = 0
        by_score = []
        for book in self.booklist:
            if book.year is None:
                continue
            if book.year < Y1 or book.year >= Y2:
                continue
            total += 1
            x = book.hit_para[key]
            if x > 0:
                t = book.npara
                f = x/t
                by_score.append((-f, book.year, book.estcid))
        by_score.sort()
        cut = round(TOP * total)
        by_year1 = collections.defaultdict(set)
        for f, year, estcid in by_score[:cut]:
            by_year1[year].add(estcid)

        by_year_range = collections.defaultdict(set)
        for y1 in RANGES:
            for year in range(y1, y1+YEARS):
                by_year_range[y1] |= by_year1[year]
        self.hits_by_year_range[key] = by_year_range

    def analyse2(self, c1, w1, c2, w2):
        print(w1, w2)
        k1 = (c1, w1)
        k2 = (c2, w2)
        result = []
        for y1 in RANGES:
            h1 = self.hits_by_year_range[k1][y1]
            h2 = self.hits_by_year_range[k2][y1]
            t = self.total_by_year_range[y1]
            n1 = len(h1)
            n2 = len(h2)
            n12 = len(h1 & h2)
            result.append([y1, y1+YEARS, t, n1, n2, n12])
        self.results.append([c1,w1,c2,w2,result])

    def dump(self, filename):
        print(filename)
        with open(filename, 'w') as f:
            json.dump(self.results, f, sort_keys=True, indent=1)

def main():
    c = Correlate()
    c.book_file('datafiles/book_metadata.json')
    c.calc_total()
    MED = ['hurtful', 'alteration', 'danger', 'poison']
    OBJ = ['coffee', 'tea', 'wine']
    for cat, words in [
        ['med', MED],
        ['object', OBJ],
    ]:
        for word in words:
            c.hitfile(cat, word, 'Csv files (research words)/{} .csv'.format(word))
            c.calc(cat, word)
    for mw in MED:
        for ow in OBJ:
            c.analyse2('med', mw, 'object', ow)
    c.dump('datafiles/correlation.json')

main()
