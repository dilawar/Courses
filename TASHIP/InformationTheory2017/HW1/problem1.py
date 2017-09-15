"""problem1.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import matplotlib.pyplot as plt
import numpy as np

def subsets( n, m ):
    ss = [ ]
    for x in range( m ):
        s = [ ]
        ss.append( s )
    return ss

def multAndAdd( l1, l2 ):
    s = 0
    for i, j in zip( l1, l2):
        s += i * j
    return s

def main( ):
    nH = 8
    pc = list( map( lambda x: 2 ** - x, range(5) ) )
    # divide 8 horses into 3 sets
    ss = subsets( nH )
    for i, s in enumerate( ss ):
        w = multAndAdd(s, pc)
        print( w, s )




if __name__ == '__main__':
    main()
