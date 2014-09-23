#!/usr/bin/env python
"""analyze.py: 

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
import random
from collections import Counter
import pylab

freq = []
freqOfA = []

def process(dir):
    files = []
    for d, sd, fs in os.walk(dir):
        for f in fs:
            files.append(os.path.join(d, f))
    processFiles(files)

def processFiles(files):
    for f in files:
        with open(f, 'r') as ff:
            processText(ff.read())

def getChunks(list, chunkSize = 100):
    chunks = []
    counter = 0
    chunk = []
    for c in list:
        if counter < chunkSize:
            counter += 1
            chunk.append(c)
        else:
            counter = 0
            chunks.append(chunk[:])
            chunk = []
    return chunks

def plot(counter):
    pylab.plot(list(counter), counter.values(), '*')
    pylab.xlim([0,20])
    pylab.show()

def processText(txt):
    print("[INFO] Processing text")
    import re
    #if len(txt) < 1000:
    #    return 
    #txt = txt[:1000]
    txt = re.sub(r'\s+', '', txt)
    m = re.findall(r'qu', txt)
    if m:
        print m, len(m)
        freqOfA.append(len(m))
    # Now we have frequencies of characters in files. Get the probability of
    # getting n-a's in a bin.

def main():
    dirName = sys.argv[1]
    process(dirName)
    print Counter(freqOfA)
    plot(Counter(freqOfA))

if __name__ == '__main__':
    main()
