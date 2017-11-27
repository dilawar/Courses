"""solvec.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import math
import matplotlib.pyplot as plt
import numpy as np

def solve( n ):
    # enumerate number of strings with given property.
    typeclass, nas  = [ ], range( 2, n/3)
    with open( '_typeclass%d.txt' % n, 'a' ) as f:
        f.write( 'x typeclass\n' )

    for an in nas:
        numOfAn = 0
        for bn in range(2*an, n-an+1):
            rest = n - an - bn
            numOfAn += math.factorial( n ) / math.factorial( an ) \
                    / math.factorial(bn) / math.factorial(rest )
        typeclass.append( numOfAn )
        with open( '_typeclass%d.txt' % n, 'a' ) as f:
            f.write( '%d %d\n' % (an, numOfAn ) )

    print( 'Wrote to _typeclass%d.txt' % n )
    print( 'Prob %g' % (1.0 * sum(typeclass) / (6.0**n) ) )

def main( ):
    solve( n = 250 )

if __name__ == '__main__':
    main()
