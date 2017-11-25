"""solveb.py: 

Solve b.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import random
import math
import numpy as np

def generate_probs( ):
    base = 1/6.0
    # p2 is 2 * p1
    p = ( np.random.random( 6 ) - 0.5 ) / 20.0
    p += base 

    # second one is always twice as big as first one plus some noise.
    while p[1] < 2*p[0]:
        delta = random.random( ) / 100.0
        p[1] += delta
        p[ random.choice( [0,2,3,4,5] ) ] -= delta
    return list(p)

def KL_divergence( P, Q ):
    dist = 0.0
    for p, q in zip( P, Q):
        dist -= p * math.log( q / p )
    return dist

def main( ):
    Q = [1/6.0] * 6
    results = [ ]
    for i in range( 1000 ):
        P = generate_probs( )
        d = KL_divergence( P, Q )
        results.append( (P, d ) )

    results.sort( )
    print( results[-1] )

if __name__ == '__main__':
    main()
