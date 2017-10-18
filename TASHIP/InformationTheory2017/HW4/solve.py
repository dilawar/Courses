"""solve.py: 

"""
from __future__ import print_function, division
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.tri as tri
mpl.rcParams['axes.linewidth'] = 0.2
mpl.rcParams['lines.linewidth'] = 1.0
mpl.rcParams['text.latex.preamble'] = [ r'\usepackage{siunitx}' ]
mpl.rcParams['text.usetex'] = True
import numpy as np
import math
from collections import Counter, defaultdict
import scipy.special

fac = scipy.special.factorial

N = 12

def abcToxy( a, b, c ):
    x = b + c/2.0
    y = ( 3 ** 0.5 ) * c / 2.0
    return (x,y)

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

def prob_num_seq( probs ):
    global N
    a, b,  c = probs
    ea, eb, ec = [ N * x for x in probs ]
    na, nb, nc = [ int(N*x) for x in probs ]
    nc = N - na - nb
    ns = [ N, N-na, N-na-nb ]
    ks = [ na, nb, nc ]
    nTS = np.prod( scipy.special.comb( ns, ks ) )
    pTS = a**na * b**nb * c**nc
    return pTS, nTS

def sequence( probs ):
    global N
    pTS, nTS = prob_num_seq( probs )
    #print( '%.5g, %.5g, %.5g' % (nTS, pTS, nTS * pTS) )
    return nTS * pTS

def typical_sequence( probs, pRef ):
    global N
    ps = np.prod( np.array( pRef ) ** ( np.array( probs ) * N ))
    ns = [ np.rint(N*x) for x in probs ]
    ns[-1] = ns[-1] + N - sum( ns )
    assert sum( ns ) == N
    ns = fac( N ) / np.prod( [ fac(x) for x in ns ] )
    return ps, ns

def abc2xy( p ):
    a, b, c = p
    return b + c/2, (3**0.5 * c)/2

def plot_ternary( pts, values, ax ):
    xys = [ abc2xy( p ) for p in pts ]
    xs, ys = zip( *xys )
    xi = np.arange( 0, 1.0, 0.001 )
    yi = np.arange( 0, 1.0, 0.001 )
    zi = mpl.mlab.griddata( xs, ys, values, xi, yi, interp='linear' )
    cs = ax.contour( xi, yi, zi, 10 )
    print( cs.levels )
    ax.clabel( cs, fontsize=8, color='white' )
    ax.set_xlim(0, 1.0 )
    ax.set_ylim(0, 1.0 )

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

    # Here we find what values of a, b, and c gives us typical sequence.
    ax2 = plt.subplot( 122 )
    pts, nts = prob_num_seq( [1/2, 1/3, 1/6] )

    dd = { }
    for a in np.arange(0, 1.1, 0.01):
        for b in np.arange(1.0-a, -0.01, -0.01):
            c = 1.0 - a - b
            probs = (a, b, c)
            ps, ns = typical_sequence( probs, [1/2, 1/3, 1/6] )
            res = ps * ns
            dd[ (a,b,c) ] = math.log( ps, 10 )

    pts, probs = list( dd.keys() ), list( dd.values( ) )
    plot_ternary( pts, probs, ax2 )

    plt.suptitle( 'N=%d, Length=%d' % (nseq,N) )
    plt.tight_layout( pad = 0.3 )
    plt.savefig( 'hw4.png' )


if __name__ == '__main__':
    main( )
