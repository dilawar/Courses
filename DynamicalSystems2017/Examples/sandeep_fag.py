"""sandeep_fag.py: 
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
import time
import numpy as np
import scipy.integrate

def plot_data( ts, ys, title, outfile = None, ax = None ):
    if ax is None:
        ax = plt.subplot( 111 )

    ax.plot( ts, ys[:,0], label = 'x' )
    ax.plot( ts, ys[:,1], label = 'y' )
    ax.legend( )
    ax.set_title( title )

    if outfile is not None:
        plt.savefig( outfile )
        plt.close( )
        print( '[INFO] Saved to %s' % outfile )

def system(t, ss):
    x, y = ss
    return [ x*(1-x)-x*y, 100*x*y - 0.1*y ]

def solve( ts, init, solver ):
    t = time.time( )
    r = scipy.integrate.ode( system )
    r.set_initial_value( init, 0 )
    r.set_integrator( solver )
    dt = ts[1] - ts[0]
    solution = [ ]
    while r.successful( ) and r.t < ts[-1]:
        r.integrate( r.t + dt )
        solution.append( r.y )
    return np.array( solution ), time.time() - t

def other_solvers( ts, init ):
    plt.figure( )
    solvers = [ 'vode', 'zvode', 'lsoda', 'dopri5', 'dop853' ]
    for i, solver in enumerate( solvers ):
        ax = plt.subplot( int(len(solvers)/2)+1, 2, i+1 )
        print( 'Solving using %s' % solver )
        ys, timeTaken = solve( ts, init, solver )
        title = '%s Time=%.4f sec' % (solver, timeTaken)
        plot_data( ts[:len(ys)], ys, title=title, ax = ax )

    plt.tight_layout( )
    plt.savefig( '%s.png' % sys.argv[0] )
    plt.close( )


def main( ):
    init = [10, 10]
    ts = np.arange( 0, 100, 0.1 )
    other_solvers( ts, init )

if __name__ == '__main__':
    main()
