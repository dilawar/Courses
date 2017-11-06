"""hw53.py: 

Solution to problem 3.

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
import random

class ZChannel( ):

    def __init__( self ):
        self.probs = { '0' : [ 1, 0 ], '1' : [ 0.5, 0.5 ] }
        self.outs = list( "01" )

    def send( self, symbols ):
        sent = [ ]
        for symbol in symbols:
            pbs = self.probs[ symbol ]
            out = np.random.choice( self.outs, p=pbs )
            sent.append( out )
        return sent

def hamming( a, b ):
    return sum( [ x != y for x, y in zip( a, b ) ] )

def decode( x, codes ):
    h = [ hamming( x, c ) for c in codes ]
    # If there is only one code with least amount of err, this is our guy, else
    # there is an error.
    countMin = h.count( min(h) )
    if countMin == 1:
        return codes[ h.index( min(h) ) ]
    return ''

def getInfo( xs, ys ):
    error = 0
    for x, y in zip( xs, ys ):
        if x != y:
            error += 1
    return ''.join(xs), ''.join(ys), error

def genCodes( M, n, size = 100 ):
    codes = [ np.random.choice( list("01"), size = n ) 
                for i in range( size ) 
                ]
    codes = [ ''.join(x) for x in codes ]
    return codes


def main( ):
    z = ZChannel( )

    R = 0.2
    ns = np.arange( 10, 210, 10 )
    res = [ ]
    for n in ns:
        M = int( 2 ** (n * R ))
        codes = genCodes( M, n, 500 )
        nErr = 0
        print( 'Total codeword sent %d, n = %d' % (len(codes),n) )
        for code in codes:
            ys = z.send( code )
            x1 = decode( ys, codes )
            if x1 != code:
                nErr += 1.0
        pErr = nErr / len( codes ) 
        print( 'Total errors %d' % nErr )
        print( 'Prob errors %f' % (nErr / len(codes)) )
        res.append( pErr )

    plt.plot( ns, res )
    plt.xlabel( 'N' )
    plt.ylabel( 'Error' )
    plt.savefig( '%s.png' % sys.argv[0] )


if __name__ == '__main__':
    main()
