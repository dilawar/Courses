"""lotteries.py: 

Two lotteris.

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

import matplotlib as mpl
import matplotlib.pyplot as plt
try:
    mpl.style.use( 'ggplot' )
except Exception as e:
    pass
mpl.rcParams['axes.linewidth'] = 0.1
plt.rc('text', usetex=True)
plt.rc('font', family='serif')

import matplotlib.pyplot as plt
import numpy as np
from collections import Counter

nTickets = 100

def playA( ):
    p = 1.0 / nTickets
    win = 0
    # Two tickets in the same lottery.
    for i in range( 2 ):
        if random.random( ) < p:
            win += 100000
            break
    return win 

def playB( ):
    p = 1.0 / nTickets
    win = 0
    # Two tickets in the different lottery.
    for i in range( 2 ):
        if random.random( ) < p:
            win += 100000
    return win 

def main( ):
    N = 1000000
    resA = [ playA( ) for i in range( N ) ]
    resB = [ playB( ) for i in range( N ) ]

    for i, res in enumerate( [ resA, resB  ] ):
        x = Counter( res )
        plt.subplot( 2, 1, i+1)

        xvec = range( len( x ) )
        yvec = np.array( x.values( ), dtype = np.float )
        yvec = yvec / np.sum( yvec )

        plt.bar( xvec, yvec, 1 )

        for xy in zip( xvec, yvec):
            plt.annotate( '%g' % xy[1], xy = xy, textcoords = 'data' )
        plt.xticks( xvec, x.keys( ) )
        plt.xlabel( 'Amount won' )
        plt.ylabel( 'Probability of winning' )
        plt.title( 'Average winning=%d' % np.mean(res) )

    plt.suptitle( 'Lottery played %d times' % N )
    plt.tight_layout( pad = 2 )
    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()
