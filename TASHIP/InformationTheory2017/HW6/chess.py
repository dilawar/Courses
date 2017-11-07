"""chess.py: 

Calculate entropy.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import math
import matplotlib.pyplot as plt
import numpy as np
import networkx as nx

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

def computeEntropy( pi, A ):
    H = 0.0
    for i, row in enumerate( A ):
        h = sum( [ -x * math.log( x, 2 ) 
                    if x != 0 else 0.0 for x in np.ravel(row) 
                    ] )
        H += pi[i] * h
    return H

def entropyRate( f ):
    print( 'Calculating entropy rate of %s' % f )
    g = nx.DiGraph( nx.drawing.nx_agraph.read_dot( f ) )
    for s, t in g.edges( ):
        if 'label' in g[s][t]:
            g[s][t]['weight'] = eval( g[s][t]['label'] )
        else:
            g[s][t]['weight'] = 0.0
    a = nx.to_numpy_matrix( g )
    stat = solveStationary( a )
    print( ' Stationary dist %s' % np.ravel(stat) )
    h = computeEntropy( stat, a )
    print( ' Entropy is %f' % h )



def main( ):
    files = [ './rook.dot', './king.dot', './bishop.dot' ]
    [ entropyRate( f ) for f in files ]

if __name__ == '__main__':
    main()
