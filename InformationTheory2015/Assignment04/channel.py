"""channel_capacity.py: 

    Compute the channel capacity for given channel described as a graph.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2015, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import networkx as nx
import numpy as np
import string
import sys
import math
from collections import Counter

def entropy(seq):
    """Compute the entropy of a sequence """
    a = Counter(seq)
    allA = sum( a.values() )
    probs = []
    for c in a:
        probs.append(float(a[c]) / allA)
    e = [ - math.log(x, 2) * float(x) for x in probs ] 
    return sum(e)

def mutual_info(seqA, seqB):
    hx = entropy(seqA)
    hy = entropy(seqB)
    hxy = entropy(zip(seqA, seqB))
    return hx + hy - hxy

class Channel():

    def __init__(self, filename):
        self.G = nx.DiGraph(nx.read_dot(filename))
        self.inputAlphabets = set()
        self.outputAlphabets = set()

    def init(self):
        print("[INFO] Initializing channel")
        for src, tgt in self.G.edges():
            self.inputAlphabets.add(src)
            self.outputAlphabets.add(tgt)
        print("[INFO] Total %s input symbols, total %s output symbol" % (
            len(self.inputAlphabets), len(self.outputAlphabets)))

    def apply_single_input(self, inAlpha ):
        outEges = self.G.successors(inAlpha)
        output = []
        for out in outEges:
             output.append((out, self.G[inAlpha][out]['prob']))
        return zip(*output)

    def simulate(self, inputs):
        output = []
        for i in inputs:
            res, p = self.apply_single_input(i)
            output += np.random.choice(res, 1, p)
        return output

    def compute(self):
        self.init()

def main():
    print("[INFO] Loading channel data from graphviz file")
    ch = Channel( sys.argv[1] )
    inputSymbols = [ 'x%s'%a for a in string.ascii_uppercase ]
    inputSeq = np.random.choice(inputSymbols, 1000)
    outputSeq = ch.simulate( inputSeq )
    ixy = mutual_info(inputSeq, outputSeq)

if __name__ == '__main__':
    main()

