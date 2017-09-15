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
meanA, meanB = [0.0], [0.0]

def playA( i ):
    global meanA
    p = 1.0 / nTickets
    win = 0
    # Two tickets in the same lottery.
    for i in range( 2 ):
        if random.random( ) < p:
            win += 100000.0
            break

    meanA.append( ( i * meanA[-1] + win ) / (i+1.0) )
    return win 

def playB( i ):
    global meanB
    p = 1.0 / nTickets
    win = 0
    # Two tickets in the different lottery.
    for i in range( 2 ):
        if random.random( ) < p:
            win += 100000.0

    meanB.append( ( i * meanB[-1] + win ) / (i+1.0) )
    return win 

def smooth( vec, N = 1000 ):
    win = np.ones( N ) / N
    return np.convolve( vec, win, 'valid' )

def main( ):
    N = 20000
    resA = [ playA( i ) for i in range( N ) ]
    resB = [ playB( i ) for i in range( N ) ]

    for i, res in enumerate( [ resA, resB  ] ):
        x = Counter( res )
        plt.subplot( 2, 2, i+1)

        xvec = range( len( x ) )
        yvec = np.array( x.values( ), dtype = np.float )
        yvec = yvec / np.sum( yvec )

        plt.bar( xvec, yvec, 1 )

        for xy in zip( xvec, yvec):
            plt.annotate( '%g' % xy[1], xy = xy, textcoords = 'data' )
        plt.xticks( xvec, x.keys( ) )
        plt.xlabel( 'Amount won' )
        plt.ylabel( 'Probability of winning' )
        plt.title( 'S%d, Average Winning=%d' % (i+1, np.mean(res) ))

    ax3 = plt.subplot( 212 )
    xvec, meanA, meanB = [ ], [ ], [ ]
    for i in np.arange( 1, len(resA), 10 ):
        xvec.append( i )
        meanA.append( np.mean( resA[:i+1] ) )
        meanB.append( np.mean( resB[:i+1] ) )

    print( 'Plotting means' )
    ax3.plot( xvec, meanA, label = r'S1' )
    ax3.plot( xvec, meanB, label = r'S2' )
    ax3.legend(loc='best', framealpha=0.4)

    plt.suptitle( 'Lottery played %d times' % N )
    plt.tight_layout( pad = 2 )
    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()
