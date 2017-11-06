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
mpl.rcParams['text.latex.preamble'] = [ r'\usepackage{siunitx}' ]
mpl.rcParams['text.usetex'] = True


transitionMat = np.matrix( 
        [ [0.25] * 4, [0.25]*4, [0.5,0.5,0,0], [0.5,0.5,0,0] ]
        )

def entropy( seq ):
    count = np.array( list( Counter( seq ).values( )))
    probs = count / sum( count )
    return sum( [ - x * math.log( x, 2 ) for x in probs ] ) 


def step( x ):
    """Take next step
    """
    r = random.random( )
    if x in "LW":
        return random.choice( list(".-") )
    return random.choice( list( ".-LW" ) )

def steps( n, init ):
    """Take n steps from init state 
    """
    traj = [ ]
    entropies = [ ]
    for i in range( n ):
        traj.append( init )
        init = step( init )
        entropies.append( entropy( traj ) )
    return traj, entropies

def solveStationary( A ):
    """ x = xA where x is the answer 

    x - xA = 0
    x( I - A ) = 0
    (I-A)^T x^T = 0
    
    """
    a = np.eye( 4 ) - A 
    a = np.vstack( (a.T, np.ones( 4 )) )
    b = np.matrix( [ 0, 0, 0, 0, 1 ] ).T
    return np.linalg.lstsq( a, b )[0]

def main( ):
    print( 'Solving ...' )
    print( transitionMat )
    res = None
    #print( transitionMat ** 2 )
    #print( transitionMat ** 10 )
    #print( transitionMat ** 20 )
    #print( transitionMat ** 100 )
    e, ev = np.linalg.eig( transitionMat )
    for i, v in zip(e,ev):
        if np.isclose(i, 1.0):
            res = v
    print( res )
    print( solveStationary( transitionMat ) )
    sol = steps( 500, '.' )
    plt.plot( sol[1] )
    plt.xlabel( 'Steps' )
    plt.ylabel( 'H(X)' )
    plt.title( 'Growth of entropy' )
    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()
