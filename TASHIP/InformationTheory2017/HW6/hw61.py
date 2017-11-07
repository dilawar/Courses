"""hw61.py: 

    Solution to problem 1.

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
import numpy as np
from collections import Counter
import matplotlib as mpl
import matplotlib.pyplot as plt
mpl.style.use( 'bmh' )
mpl.rcParams['axes.linewidth'] = 0.2
mpl.rcParams['lines.linewidth'] = 1.0
mpl.rcParams['text.usetex'] = True


transitionMat = np.matrix( 
        [ [0.25] * 4, [0.25]*4, [0.5,0.5,0,0], [0.5,0.5,0,0] ]
        )

def entropy( seq ):
    count = np.array( list( Counter( seq ).values( )))
    probs = count / sum( count )
    return sum( [ - x * math.log( x, 2 ) for x in probs ] ) 

def step( n ):
    allSeq = [ ]
    seq = [ random.choice( ".-LW" ) ]
    for i in range( n ):
        if seq[-1] in "LW":
            seq.append( random.choice( list(".-") ) )
        else:
            seq.append( random.choice( list( ".-LW" ) ) )
    return entropy( seq )

def conditionalEntropy( n ):
    """Take n teps from init state 
    """
    # We generate 1000 sequence of size n.
    H, h = [ ], 0.0
    for ii in range( n ):
        H.append( h )
        h = step( ii )
    return H

def solveStationary( A ):
    """ x = xA where x is the answer 
    x - xA = 0
    x( I - A ) = 0 and sum(x) = 1
    """
    n = A.shape[0]
    a = np.eye( n ) - A 
    a = np.vstack( (a.T, np.ones( n )) )
    b = np.matrix( [0] * n + [ 1 ] ).T
    return np.linalg.lstsq( a, b )[0]

def main( ):
    print( 'Solving ...' )
    piState = solveStationary( transitionMat )
    H = 0.0
    print( 'Stationary dist %s' % piState )
    for i, row in enumerate( transitionMat ):
        h = sum( [ -x * math.log( x, 2 ) if x != 0 else 0.0 for x in np.ravel(row) ] )
        H += piState[i] * h
    print( 'Entropy %f' % H )

    sol = conditionalEntropy( 1000 )
    plt.plot( sol )
    plt.xlabel( 'Seq Length' )
    plt.ylabel( 'H(X)' )
    plt.title( 'Growth of entropy with N' )
    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()
