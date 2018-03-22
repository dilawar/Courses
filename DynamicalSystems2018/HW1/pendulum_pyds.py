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
from PyDSTool.Toolbox import phaseplane as pp

rnd = random.random

def construct_system( I, alpha, init=(0,0), T = 10 ):
    theta0, w0 = init
    args = pd.args( name = 'Pendulum' )
    args.pars = { 'alpha' : alpha, 'I' : I }
    args.varspecs = { 'theta' : 'w', 'w' : 'I - sin(theta) - alpha * w' } 
    args.ics = { 'theta' : theta0, 'w' : w0 }
    args.tdomain = [-T, T ]
    ode = pd.Generator.Vode_ODEsystem( args )
    return ode

def compute( ode, T = 100):
    trajs = [ ]
    xs = np.arange( -2*math.pi, 2*math.pi, math.pi / 4.0 )
    initPs = itertools.product( xs, repeat = 2 )
    for theta, w in initPs:
        trajs.append( compute_traj( ode, theta, w, T ) )
    return trajs

def compute_traj( ode, theta, w, T = 40 ):
    ode.set( ics = dict( theta = theta, w = w ) )
    ode.set( tdomain = [-T, T ] )
    ode.set( tdata = [-T, T ] )
    t = ode.compute( '%f_%f' % (theta,w) ).sample( )
    return t

def compute_trajs_near( ode, theta, w, N = 10, err = 0.1, T = 10 ):
    trajs = [ ]
    for dt, dw in itertools.product( [ -0.5, -0.1, 0, 0.1, 0.5 ], repeat = 2 ):
        theta1, w1 = theta + dt, w + dw
        t = compute_traj( ode, theta1, w1, T )
        trajs.append( t )
    return trajs

def plot( trajs, ax ):
    for t in trajs:
        x, y = t['theta'], t['w']
        dr = (np.diff(x) ** 2 + np.diff( y ) ** 2) ** 0.5
        dx, dy = x[1] - x[0], y[1] - y[0]
        cax = ax.scatter( x[1:], y[1:], s=0.05, c = dr, vmin=0, vmax=0.02, cmap = 'jet')
        ax.set_xlabel( r'$\theta$' )
        ax.set_ylabel( r'$\omega$' )
        ax.set_xlim( [ -2 *math.pi, 2 *math.pi ] )

def compute_phase_plane( ode, I, alpha, ax = None ):
    ode.set( pars = dict( I = I, alpha = alpha ) )
    ode.set( xdomain = dict( theta = [-2*math.pi,2*math.pi], w = [-math.pi, math.pi] ) )
    print( '| Computing for I=%f and alpha=%f' % (I,alpha) )
    if ax is None:
        return 

    # Find fixed-points and plot phase-plots.
    pp.plot_PP_vf( ode, 'theta', 'w', N = 20, scale_exp = -0.5, alpha=0.4 )
    fixedP, nullx, nully = pp.get_PP( ode, { 'theta' : 0, 'w' : 0 }, [ 'theta', 'w' ] )
    for p in fixedP:
        ax.plot( p['theta'], p['w'], 'o', color = 'black' )
        # Now compute some trajectories near fixed-points and draw them.
        trajs = compute_trajs_near( ode, p['theta'], p['w'], err = math.pi /2.0 )
        plot( trajs, ax )

    ax.plot( *nullx.T, '-.', color = 'blue' )
    ax.plot( *nully.T, '-.', color = 'red' )
    ax.set_title( r'I=%.2f, $\alpha=%.2f$' % (I, alpha), fontsize = 8 )


def main( args = None ):
    print( 'Constructing system' )
    ode = construct_system( 0.0, 0.0 )

    ps = [ 0.01, 0.02, 0.5, 0.9 ]
    nn = math.ceil( len(ps) ** 0.5 )
    for i, I in enumerate(ps):
        outfile = '%s_%03d.png' % (sys.argv[0], i)
        plt.figure( )
        for j, alpha in enumerate(ps):
            ax = plt.subplot( nn, nn, j + 1 )
            compute_phase_plane( ode, I=I, alpha=alpha, ax = ax )
        plt.xlabel( r'$\theta$' )
        plt.ylabel( r'$\omega$' )
        plt.suptitle( r'Pendulum: $\dot{\theta}=\omega$,\quad $\dot{\omega}=I-sin(\theta)-\alpha\omega$' )
        plt.tight_layout( rect = [0,0,1,0.95] )
        plt.savefig( outfile )
        print( 'Saved to find %s' % outfile )
        plt.close( )
    
if __name__ == '__main__':
    main( )

