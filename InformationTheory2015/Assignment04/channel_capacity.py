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
import sys

class Channel():

    def __init__(self, filename):
        self.G = nx.read_dot(filename)
        self.inputAlphabets = set()
        self.outputAlphabets = set()

    def init(self):
        for src, tgt in self.G.edges():
            self.inputAlphabets.add(src)
            self.outputAlphabets.add(tgt)

    def compute(self):
        print("[INFO] Computing")
        self.init()


def main():
    print("[INFO] Loading channel data from graphviz file")
    ch = Channel( sys.argv[1] )
    ch.compute()

if __name__ == '__main__':
    main()

