"""pendulum_pyds.py: 

Using PyDSTools.

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
import random
import itertools
import PyDSTool as pd

def construct_system( I, alpha, init=(0,0), T = 5 ):
    theta0, w0 = init
    args = pd.args( name = 'Pendulum' )
    args.pars = { 'alpha' : alpha, 'I' : I }
    args.varspecs = { 'theta' : 'w', 'w' : 'I - sin(theta) - alpha * w' } 
    args.ics = { 'theta' : theta0, 'w' : w0 }
    args.tdomain = [0, T ]
    ode = pd.Generator.Vode_ODEsystem( args )
    return ode

def compute( ode, T = 40):
    trajs = [ ]
    xs = np.arange( -2*math.pi, 2*math.pi, math.pi / 4.0 )
    initPs = itertools.product( xs, repeat = 2 )
    for theta, w in initPs:
        ode.set( ics = dict( theta = theta, w = w ) )
        t = ode.compute( '%f_%f' % (theta,w) ).sample( )
        trajs.append( t )
    return trajs

def plot( trajs, ax ):
    for t in trajs:
        x, y = t['theta'], t['w']
        dx, dy = x[1] - x[0], y[1] - y[0]
        ax.plot( x, y, color = 'blue', lw = 0.5 )
        ax.set_xlabel( r'$\theta$' )
        ax.set_ylabel( r'$\omega$' )
        ax.set_xlim( [ -2 *math.pi, 2 *math.pi ] )

def compute_phase_plane( ode, I, alpha, ax = None ):
    ode.set( pars = dict( I = I, alpha = alpha ) )
    print( '| Computing for I=%f and alpha=%f' % (I,alpha) )
    trajs = compute( ode )
    if ax is None:
        return 
    plot( trajs, ax)
    ax.set_title( r'I=%.2f, $\alpha=%.2f$' % (I, alpha), fontsize = 8 )


def main( args = None ):
    print( 'Constructing system' )
    ode = construct_system( 0.0, 0.0 )
    ps = [ 0.0, 0.01, 0.1, 0.5, 1.0 ]
    plt.figure( figsize = (len(ps)*2, len(ps)*2 ) )
    for i, (I, alpha) in enumerate(itertools.product(ps, repeat = 2)):
        ax = plt.subplot( len(ps), len(ps), i + 1 )
        compute_phase_plane( ode, I=I, alpha=alpha, ax = ax )

    plt.xlabel( r'$\theta$' )
    plt.ylabel( r'$\omega$' )
    plt.tight_layout( )
    plt.savefig( '%s.png' % sys.argv[0] )
    print( 'Saved to find %s.png' % sys.argv[0] )
    
if __name__ == '__main__':
    main( )

