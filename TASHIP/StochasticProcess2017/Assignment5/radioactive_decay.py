"""radioactive_decay.py: 

Radio avtive decay.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2016, Dilawar Singh"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import matplotlib.pyplot as plt
import random
import numpy as np

rate_of_decay_ = 0.01


class Lump( ):

    """A radioactive lump of size N"""

    def __init__(self, N):
        self.N = N
        self.rate_of_decay = rate_of_decay_
        self.dt = 0.1
        
    def decay( self ):
        a = random.random( )
        ndecay = 0
        rands = np.random.random( self.N )
        ndecay = len( rands[ np.where( rands < self.dt * self.rate_of_decay ) ])
        self.N -= ndecay
        return self.N

def main( ):

    lumps = [ ]
    nLumps, nNuc = 1000, 1000
    for i in range( nLumps ):
        l = Lump( nNuc )
        lumps.append( l )

    xvec, yvar, ysum = [ ], [ ], [ ]
    for i in range( 3000 ):
        xvec.append( i )
        currentNum = []
        for l in lumps:
            currentNum.append( l.decay( ) )
        ysum.append( sum( currentNum ) )
        yvar.append( np.std( currentNum ) )

    plt.subplot( 211 )
    plt.plot( xvec, yvar, label = 'Variance' )
    plt.legend( )
    plt.subplot( 212 )
    plt.plot( xvec, ysum, label = 'Decay' )
    plt.legend( )
    plt.suptitle( 'Total %d lumps. %d in each lump' % (nLumps, nNuc ))
    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()

        
