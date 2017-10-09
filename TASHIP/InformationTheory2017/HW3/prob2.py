"""prob2.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import itertools
import random
import operator
import numpy as np
from functools import reduce
from collections import Counter

dist_ = { 'A' : 1/2.0, 'T' : 1/4.0, 'G' : 1/8.0, 'C' : 1/8.0 }
vars_ = dist_.keys( )

def seqToStr( seq ):
    return ''.join( seq )

def seq_prob( seq ):
    prob = reduce( operator.mul, [ dist_[x] for x in seq ] )
    return prob

def is_typical( seq, dist ):
    """In a typical seq, letters occur as expected
    """
    d = Counter( seq )
    isTypical = True
    for v in vars_:
        if d.get(v, 0.0) / len(seq) != dist[v]:
            isTypical = False
    return isTypical

def number_of_typical( size ):
    letters = list( 'ATGC' )
    typicals = [ ]
    N = 100000
    for i in range( N ):
        seq = np.random.choice(letters, size, p = [1/2.0,1/4.0,1/8.0,1/8.0] )
        if is_typical( seq, dist_ ):
            typicals.append( seq )
    print( len(typicals) / N )

def main( ):
    number_of_typical( 8 )


if __name__ == '__main__':
    main()
