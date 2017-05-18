#!/usr/bin/env python3

import collections
import csv
import json
import math

Y1 = 1700
Y2 = 1800
YEARS = 10

RANGES = list(range(Y1, Y2-YEARS+1))

class Analyse:
    def __init__(self):
        pass

    def load(self, filename):
        print(filename)
        with open(filename) as f:
            self.results = json.load(f)

    def analyse(self, filename):
        print(filename)
        with open(filename, 'w', newline='') as f:
            writer = csv.writer(f)
            for c1, w1, c2, w2, result in self.results:
                self.analyse2(writer, c1, w1, c2, w2, result)

    def analyse2(self, writer, c1, w1, c2, w2, result):
        for y1, y2, nxx, n1x, nx1, n11 in result:
            n10 = n1x - n11
            n01 = nx1 - n11
            n0x = nxx - n1x
            nx0 = nxx - nx1
            n00 = nxx - n01 - n10 - n11
            phi = (n11 * n00 - n10 * n01) / math.sqrt(n1x * n0x * nx1 * nx0)
            f1x = n1x / nxx
            fx1 = nx1 / nxx
            f11 = n11 / nxx
            writer.writerow([w1, w2, y1, y2, f1x, fx1, f11, f1x*fx1, phi])

def main():
    a = Analyse()
    a.load('datafiles/correlation.json')
    a.analyse('datafiles/correlation.csv')

main()
