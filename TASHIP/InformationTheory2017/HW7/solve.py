"""solve.py: 

HW7 solution.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import numpy as np

def roll_die( n ):
    return np.random.randint( low = 0, high = 7, size = n )

def twoVsOne( vec ):
    nTwo, nOne = len( vec[vec==2] ), len( vec[vec==1] )
    return nTwo / nOne

def main( ):
    N = 10 ** 4
    nRare = 0.0

    # Run till 100 rare events are found.
    while nRare < 100.0:
        r = roll_die( 1000 )
        if twoVsOne( r ) >= 2.0:
            nRare += 1.0
            print( 'Ah, A rare event' )

    print( "Prob of rare event: %f" % (nRare / N ) )


if __name__ == '__main__':
    main()
