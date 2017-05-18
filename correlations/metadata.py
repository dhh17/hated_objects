#!/usr/bin/env python3

import collections
import csv
import json
import re

def fix_estcid(x):
    assert x[0] in 'NPRTW', x
    return x[0] + re.sub(r'^0+', '', x[1:])

class Book:
    def __init__(self, estcid):
        self.estcid = estcid
        self.nchar = 0
        self.npara = 0
        self.npage = 0
        self.year = None

    def sortkey(self):
        y = self.year
        if y is None:
            y = 9999
        return (y, self.estcid)

class Process:
    def __init__(self):
        self.books = {}

    def book_length_file(self, filename):
        print(filename)
        with open(filename, newline='') as f:
            reader = csv.reader(f)
            header = next(reader)
            assert header == ['', 'ESTCID', 'documentLength', 'totalParagraphs', 'totalPages']
            for hitnum, estcid, nchar, npara, npage in reader:
                estcid = fix_estcid(estcid)
                if estcid not in self.books:
                    book = Book(estcid)
                    self.books[estcid] = book
                book = self.books[estcid]
                book.nchar += int(nchar)
                book.npara += int(npara)
                book.npage += int(npage)
        print(len(self.books))

    def book_metadata_file(self, filename):
        print(filename)
        with open(filename, newline='') as f:
            reader = csv.reader(f)
            header = next(reader)
            headermap = { h:i for i,h in enumerate(header) }
            for row in reader:
                estcid = row[headermap['id']]
                if estcid not in self.books:
                    continue
                book = self.books[estcid]
                year = row[headermap['publication_year']]
                year = None if year == 'NA' else int(year)
                assert book.year is None or book.year == year
                book.year = year

    def dump(self, filename):
        print(filename)
        books = list(self.books.values())
        books.sort(key=lambda book: book.sortkey())
        results = []
        for book in books:
            assert book.nchar > 0
            results.append({
                'estcid': book.estcid,
                'nchar': book.nchar,
                'npara': book.npara,
                'npage': book.npage,
                'year': book.year,
            })
        with open(filename, 'w') as f:
            json.dump(results, f, sort_keys=True, indent=1)

def main():
    p = Process()
    p.book_length_file('datafiles/ecco_lengths.csv')
    p.book_metadata_file('datafiles/estc_processed_080517.csv')
    p.dump('datafiles/book_metadata.json')

main()
