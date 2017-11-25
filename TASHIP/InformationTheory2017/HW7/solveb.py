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
import PyGnuplot as pg

def entropy( ps ):
    return sum([ - math.log( p, 2.0 ) * p for p in ps ])

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

    # renormalize
    p = p / sum( p )
    assert p.all( ) > 0.0
    return list(p)

def KL_divergence( P, Q ):
    dist = 0.0
    for p, q in zip( P, Q):
        dist -= p * math.log( q / p )
    assert dist >= 0.0, (dist, P, Q)
    return dist

def main( ):
    Q = [1/6.0] * 6
    results = [ ]
    with open( "tmp.dat", "w" ) as  f:
        for i in range( 10000 ):
            P = generate_probs( )
            d = KL_divergence( P, Q )
            results.append( (P, d, entropy(P) ) )
            f.write( "%g %g\n" % (d, entropy(P)) )

    pg.c( 'set xlabel "H(E)"' )
    pg.c( 'set ylabel "D(E||Q), Q is uniform"' )
    pg.c( 'set terminal pdf; set output "problem2.pdf"; ' )
    pg.c( 'plot "tmp.dat" using 1:2 w p title "KL distance V/S entropy"' )


if __name__ == '__main__':
    main()
