"""solve.py: 

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
import math
import matplotlib.tri as tri
from collections import Counter, defaultdict
import scipy.special

fac = math.factorial

N = 16

def plot_ternary( pts, v, ax = None ):
    # translate the data to cartesian corrds
    a, b, c = [ np.array(x) for x in zip( *pts ) ]
    x = 0.5 * ( 2.*b+c ) / ( a+b+c )
    y = 0.5*np.sqrt(3) * c / (a+b+c)

    # create a triangulation out of these points
    T = tri.Triangulation(x,y)

    # plot the contour
    if ax is None:
        ax = plt.subplot( 111 )

    f = ax.tricontourf(x,y,T.triangles,v)
    plt.colorbar( f, ax = ax )

    # create the grid
    corners = np.array([[0, 0], [1, 0], [0.5,  np.sqrt(3)*0.5]])
    triangle = tri.Triangulation(corners[:, 0], corners[:, 1])

    # creating the grid
    refiner = tri.UniformTriRefiner(triangle)
    trimesh = refiner.refine_triangulation(subdiv=4)

    #plotting the mesh
    ax.triplot(trimesh, ',')
    ax.axis( 'off' )

    return ax

def generate_seqs( probs, n ):
    global N
    chars = [ 'A', 'B', 'C' ]
    seqs = [ np.random.choice( chars, p = probs, size=N ) for i in range( n ) ]
    return seqs

def compute_ternary( seqs ):
    chars = "ABC"
    probs = defaultdict( int )
    for seq in seqs:
        c = Counter( seq )
        pbs = [ c[x] / N for x in chars ]
        probs[ tuple(pbs) ] += 1
    return probs

def sequence( probs ):
    global N
    a, b, c = probs
    ea, eb, ec = [ N * x for x in probs ]
    na, nb, nc = [ int(N*x) for x in probs ]
    nc = N - na - nb
    ns = [ N, N-na, N-na-nb ]
    ks = [ na, nb, nc ]
    nTS = np.prod( scipy.special.comb( ns, ks ) )
    pTS = a**na * b**nb * c**nc
    print( '%.5g, %.5g, %.5g' % (nTS, pTS, nTS * pTS) )
    return nTS * pTS

def main( ):
    points, vals = [ ], [ ]
    for a in np.arange(0, 1.1, 0.1):
        for b in np.arange(1.0-a, -0.1, -0.1):
            c = 1.0 - a - b
            probs = (a, b, c)
            p = sequence( probs )
            points.append( probs )
            vals.append( p )

    plt.figure( figsize=(8,3) )

    ax1 = plt.subplot( 121 )
    # for given probs generate many seqs
    pbs = [ 1/2, 1/3, 1/6 ]
    nseq = 10000
    seqs = generate_seqs( pbs, n = nseq )

    data = compute_ternary( seqs )


    pts, vals = list(data.keys( )), list(data.values( ))
    plot_ternary( pts, vals, ax1 )
    ax1.set_title( 'Number of seq in class' )
    ax1.annotate( '1/2,1/3,1/6', xy=(5/12, 1.732/12), xytext=(0.5,0.5)
            , arrowprops = dict( facecolor='black', arrowstyle='->' )
            )

    ax2 = plt.subplot( 122 )
    probs = [ math.log( x / sum(vals), 2) for x in vals ]
    plot_ternary( pts, probs, ax2 )

    plt.suptitle( 'N=%d, Length=%d' % (nseq,N) )
    plt.tight_layout( pad = 0.3 )
    plt.savefig( 'hw4.png' )


if __name__ == '__main__':
    main()
