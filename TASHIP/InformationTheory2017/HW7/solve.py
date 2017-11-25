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
    return (nOne, nTwo)

def main( ):
    nRare = 0.0
    # Run till 100 rare events are found.
    t = time.time( )
    with open( 'results.txt', 'w' ) as f:
        f.write( 'n N nE n1 p t\n' )

    for n in range(10, 5001, 50):
        t = time.time( )
        N, nRare = 0.0, 0.0
        n1InRateStrings = [ ]
        while nRare < 100.0:
            N += 1
            try:
                n1, n2 = twoVsOne( roll_die( n ) )
                if (n2/n1) >= 2.0:
                    nRare += 1.0
                    n1InRateStrings.append( 1.8 * n1 / n )
            except Exception as e:
                pass

        ttaken = time.time( ) - t
        with open( 'results.txt', 'a' ) as f:
            f.write( '%f %f %f %f %f %f\n' % (n,N,nRare
                ,np.mean(n1InRateStrings),1.0*nRare/N,ttaken) )
        print( "Prob of rare event: %f" % (nRare / N ) )
        print( 'Time taken %f seconds' % (time.time() - t ) )
    print( 'All done' )


if __name__ == '__main__':
    main()
