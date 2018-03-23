"""pendulum.py: 

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

import os
import sys
import matplotlib as mpl
import matplotlib.cm as cm
import matplotlib.pyplot as plt

try:
    mpl.style.use( 'ggplot' )
    mpl.rcParams['text.usetex'] = True
except Exception as e:
    print( e )

import scipy.integrate as sci
import numpy as np
import math
import random
import itertools
from collections import defaultdict

import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt

def change_sign( vec ):
    # use np.signbit instead of np.sign
    # this is not the best way to compute the zero-crossing. 
    #
    # For b = [ 1, 2, 0, -1, 0, 0, -1, 2], it returns 3 crossing though there
    # are only 2.
    idx = np.signbit( vec ) != np.signbit( np.roll(vec, 1))
    return np.where( idx )[0][1:]

def isclose( v1, v2, eabs = 1e-3 ):
    return abs( v1 - v2 ) < eabs

def is_periodic_helper( vec ):
    # List of timeperiods. It may contain 1 timeperiod of multiple timeperid.
    # Essentially we are looking of repeating substring.
    tps = vec.copy( )
    res = [ ]
    while sum(tps) > 1:
        x0 = tps[np.nonzero(tps)][0]
        ts = [ i for i, x in enumerate(tps) if isclose(x,x0,max(3,0.1*x0)) ]
        if not ts:
            continue
        res.append( (x0, np.mean(np.diff(ts))) )
        tps[ts] = 0

    periods, periodicity = zip( *res )
    if min(periodicity) == max(periodicity):
        return np.sum( periods )
    return 0

def is_periodic( vec, polar = False, ax = None):
    # normalize the vector
    if polar:
        vec[:,0] = vec[:,0] % (2*np.pi) 
    ref = vec[-1]
    dist = np.zeros( len(vec) )
    phase = np.zeros( len(vec) )
    periods = [ ]
    prevI = 0
    for i, v in enumerate(vec[::-1]):
        dist[i] = sum( (v - ref) ** 2) ** 0.5
        phase[i] = math.atan(v[1]/v[0]) - math.atan(ref[1] / ref[0])

    # normalize distance.
    if dist.max( ) == dist.min():
        return False

    if phase.max() == phase.min():
        return False

    if ax is not None:
        ax.plot( phase ) #, color = 'blue' )
        ax.plot( dist ) #, color = 'red' )

    zeroCross = change_sign( phase )
    valsAtZero = dist[zeroCross]

    # remove zero-crossings where distance is not close to zero.
    goodPoints = [ x for x in zip(zeroCross, valsAtZero) if not isclose(x[1],0) ]
    if len( goodPoints) < 1:
        return False

    zeroCross, valsAtZero = zip(*goodPoints)
    if len(zeroCross) < 2:
        return 0

    timePeriod = np.diff( zeroCross )
    return is_periodic_helper( timePeriod )


def rnd( ):
    return random.uniform( -math.pi, math.pi )

def build_system( I, alpha ):

    def pend( state, t ):
        theta, w = state
        dtheta = w 
        dw = I - math.sin( theta ) - alpha * w
        return dtheta, dw
    return pend

def get_trajectory( system, init, tvec = [ ] ):
    if not tvec:
        tvec = np.arange( 0, 1000, 0.1 )
    y = sci.odeint( system, init, tvec )
    return tvec, y

def plot_on_cylinder( x, y, ax ):
    ax.plot( np.cos(x), np.sin(x), y, lw = 1.5 )

def main( ):

    alphas = np.array( [0.01, 0.1, 0.5 ] )
    Is = 4 * alphas / math.pi
    plotOnCyl = True

    #  for i,(I,alpha) in enumerate( itertools.product( Is, alphas ) ):
    for i, (I, alpha) in enumerate( zip(Is, alphas) ):
        gridSize = (2, 2)
        ax1 = plt.subplot2grid( gridSize, (0,0), colspan = 1, projection = '3d' )
        ax2 = plt.subplot2grid( gridSize, (0,1), colspan = 1 )
        ax3 = plt.subplot2grid( gridSize, (1,0), colspan = 1, projection='polar' )
        ax4 = plt.subplot2grid( gridSize, (1,1), colspan = 1 )
        system = build_system( I, alpha )

        # Only plot is plotted in spherical coordinate system. Most plots are on
        # x-y plane.
        for ii in range( 5 ):
            t, ys = get_trajectory( system, [ rnd(), rnd() ] )
            #  ys[:,0] = ys[:,0] / (2*math.pi)
            x, y = ys[:,0], ys[:,1]
            if ii < 2:
                T = is_periodic( ys[-500:], polar = True )
                if T:
                    print( 'Peridic trajectory is found with periodic %f' % T )
            if ii < 1:
                plot_on_cylinder( x, y, ax1 )
            #  c = cm.jet( ii / 10.0 )
            ax2.plot( x, y, lw = 0.5 )
            #  ax2.scatter( x[0], y[0], s=0.8, color = c )
            ax3.plot( x % (2*math.pi), y )
            dr = (np.diff(x) ** 2 + np.diff(y) ** 2) ** 0.5

        ax2.set_xlabel( r'$\theta$' )
        ax2.set_ylabel( r'$\omega$' )
        ax1.set_xlabel( r'$sin(\theta)$' )
        ax1.set_ylabel( r'$cos(\theta)$' )
        plt.suptitle( r'$I=%f,\, \alpha=%s$' % (I, alpha) )
        outfile = os.path.join( './figures', 'I%f_a%f.png' % (I,alpha) )
        plt.tight_layout( rect = (0,0,1,0.95) )
        plt.savefig( outfile )
        plt.close( )
        print( 'Saved to %s' % outfile )

def test( ):
    a = [ 1, 2, 3, -1, -1, 2, 1, -2, -2, 0, 1, 1 ]
    b = [ 1, 2, 0, -1, 0, 0, -1, 2]
    res = [ (x, change_sign(x)) for x in [a, b ] ]
    print( res )

if __name__ == '__main__':
    #  test( )
    main()
