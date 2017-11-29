"""solvec.py: 

"""
from __future__ import division

__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import math
import numpy as np
import random
import sample_simplex as ss

def prob_p_when_q_is_known( n, P, Q ):
    hpx = sum( [ - p * math.log(p,2.0) for p in P ] ) 
    dpq = sum( [ p * math.log( p/q, 2.0 ) for (p,q) in zip(P,Q) ] )
    res = 2 ** (- n * (hpx + dpq))
    return res

def solve( n ):
    # enumerate number of strings with given property.
    typeclass, nas  = [ ], range( 2, int(n/3) )
    with open( '_typeclass%d.txt' % n, 'a' ) as f:
        f.write( 'x typeclass\n' )

    for an in nas:
        numOfAn = 0.0
        for bn in range(2*an, n-an+1):
            rest = n - an - bn
            numOfAn += math.factorial( n ) // math.factorial( an ) \
                    // math.factorial(bn) // math.factorial(rest )
        typeclass.append( numOfAn )
        with open( '_typeclass%d.txt' % n, 'a' ) as f:
            f.write( '%d %d\n' % (an, numOfAn ) )

    print( 'Wrote to _typeclass%d.txt' % n )
    print( 'Prob %g' % (1.0 * sum(typeclass) / (6.0**n) ) )

    # using theorem 11.1.2
    U = [ 1/6.0 ] * 6

    with open( 'probd.txt', 'w' ) as f:
        f.write( 'n commutative_prob\n' )

    allprob = 0.0
    for i in range( 10**5 ):
        vec = ss.sample_simplex_uniform( 6 )
        while vec[1] < 2 * vec[0]:
            # randomly select an indices
            idx = random.choice( range(6) )
            d = vec[idx] / 10.0
            vec[ idx ] -= d 
            vec[1] += d 
        allprob += prob_p_when_q_is_known(n, vec, U)
        with open( 'probd.txt', 'a' ) as f:
            f.write( '%d %g\n' % (i, allprob ) )
    print( 'All done. Saved data to probd.txt' )

def main( ):
    solve( n = 250 )

if __name__ == '__main__':
    main()
