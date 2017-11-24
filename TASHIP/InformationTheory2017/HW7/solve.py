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
import time
import os
import numpy as np

def roll_die( n ):
    return np.random.randint( low = 0, high = 7, size = n )

def twoVsOne( vec ):
    nTwo, nOne = len( vec[vec==2] ), len( vec[vec==1] )
    return nTwo / nOne

def main( ):
    nRare = 0.0
    # Run till 100 rare events are found.
    t = time.time( )
    with open( 'results.txt', 'w' ) as f:
        f.write( 'n, N, rare, prob, time\n' )

    for n in range(10, 5001, 50):
        t = time.time( )
        N, nRare = 0.0, 0.0
        while nRare < 100.0:
            r = roll_die( n )
            N += 1
            try:
                if twoVsOne( r ) >= 2.0:
                    nRare += 1.0
                    print( '%d Ah, A rare event' % N )
            except Exception as e:
                pass

        ttaken = time.time( ) - t
        with open( 'results.txt', 'a' ) as f:
            f.write( '%d, %d, %d, %f, %f\n' % (n, N, nRare, nRare/N, ttaken) )
        print( "Prob of rare event: %f" % (nRare / N ) )
        print( 'Time taken %f seconds' % (time.time() - t ) )
    print( 'All done' )


if __name__ == '__main__':
    main()
