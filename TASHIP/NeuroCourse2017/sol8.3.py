"""sol8.3.py: 

Summation on neuron.

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
import numpy as np
import math

grid = np.arange( 50,70, 1 )
synapses = [ ]
velocity = 1.0

def signal( tvec, startT, viewT ):
    global velocity
    assert viewT >= startT, "Cant view signal before its applied"
    diffT = viewT - startT
    x =  diffT  / velocity
    tau = 1 / 8.0
    spread = (1+x) ** 0.5
    return np.exp( - ( tvec - startT ) ** 2 / spread ) * math.exp( - diffT * tau )


def main( N ):
    dist = np.random.choice( grid, N )
    time = np.random.choice( np.arange(0,10,1), N )
    tvec = np.arange( -20, 20, 0.1 )
    outputs = [ ]
    for i in range( N ):
        x0 = dist[ i ]
        t0 = time[ i ]
        viewT = x0 / velocity 
        # print( x0, t0, viewT )
        output = signal( tvec, t0, viewT )
        outputs.append( output )

    soma = np.sum( outputs, axis = 0 )
    plt.figure( )
    plt.plot( tvec, soma, label = 'Sum' )
    plt.plot( tvec, [ 0.5 ] * len( tvec ), label = 'Threshold' )
    plt.legend( )
    plt.ylabel( 'Sum at soma' )
    plt.xlabel( 'Time' )
    plt.title( 'N = %d' % N )
    outfile = 'prob83_N%d.png' % N
    plt.savefig( outfile )
    print( '[INFO] Saved to %s' % outfile )
    plt.close( )
    return soma.max( )

def test( ):
    tvec = np.arange( 0, 20, 0.1 )
    plt.plot( tvec, signal( tvec, 0, 0 ) )
    plt.plot( tvec, signal( tvec, 0, 4 ) )
    plt.plot( tvec, signal( tvec, 2, 2 ) )
    plt.plot( tvec, signal( tvec, 2, 4 ) )
    plt.show( )

if __name__ == '__main__':
    # test( )
    nvec, resvec = [ ], [ ]
    for i in range( 10, 21 ):
        N = int( 10 ** (i/10.0 ) )
        res = main( N )
        nvec.append( N ); resvec.append( res )

    plt.figure( )
    plt.plot( nvec, resvec )
    plt.xlabel( 'Number of convergent connections' )
    plt.ylabel( 'Response at soma' )
    plt.savefig( 'prob83_N_final.png' )
