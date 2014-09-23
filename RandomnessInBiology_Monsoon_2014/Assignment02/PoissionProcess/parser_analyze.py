"""parser_analyze.py: Parse and analyze monsson data.

Last modified: Sat Jan 18, 2014  05:01PM

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2013, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import os
import sys
import re
import csv
from collections import Counter
from scipy.stats import poisson

rainfall = {}

def getData(year, month, binsize = 7*4):
    data = []
    result = []
    for district, y in rainfall:
        if int(y) == year:
            r = rainfall[(district, y)]
            for i, p in enumerate(r):
                p = str(p)
                if 'N.A.' in p:
                    try:
                        r[i] = float(r[i-1]) + float(r[i+1]) / 2.0
                    except:
                        try:
                            r[i] = float(r[i-1])
                        except:
                            try:
                                r[i] = float(r[i+1])
                            except:
                                r[i] = 0.0

            if r: data.append(r)
    for d in data:
        result.append(float(d[month-1]))
    result = [ int(x/binsize) for x in result]
    return result

def plot(counter):
    import pylab
    x = list(counter)
    y = counter.values()
    totalY = sum(y)
    y = [ float(r) / totalY for r in y ]
    # mean and std-deviation
    fig, ax = pylab.subplots()
    ax.bar(x, y, color='y')
    ax.plot(x, poisson.pmf(x, 4), color='b')
    pylab.show()

def process():
    print("Processing ...")
    data = []
    for month in [5,6,7,8,9]:
        for year in [2004, 2005, 2006, 2007, 2008, 2009 ]:
            data += getData(year, month)
    print data
    plot(Counter(data))

def main(sheetpath):
    print("Opening sheets")
    with open(sheetpath, 'r') as f:
        file = csv.reader(f)
        for row in file:
            rainfall[(row[1], row[2])] = row[4:]
    process()

if __name__ == '__main__':
    sheet = sys.argv[1]
    main(sheet)
