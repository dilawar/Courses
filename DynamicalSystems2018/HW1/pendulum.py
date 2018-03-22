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

import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt

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
    space = [0.01, 0.1, 0.5 ]
    plotOnCyl = True
    for i,(I,alpha) in enumerate( itertools.product( space, repeat = 2)):
        plt.figure( figsize=( 5, 3 ) )
        system = build_system( I, alpha )
        ax1 = plt.subplot( 1, 2, 1, projection = '3d' )
        ax2 = plt.subplot( 1, 2, 2 )

        # Only plot is plotted in spherical coordinate system. Most plots are on
        # x-y plane.
        for ii in range( 30 ):
            t, ys = get_trajectory( system, [ rnd(), rnd() ] )
            x, y = ys[:,0], ys[:,1]
            if ii < 1:
                plot_on_cylinder( x, y, ax1 )
            c = cm.hot( ii / 30.0 )
            ax2.plot( x, y, lw = 0.5, color = c )
            ax2.scatter( x[0], y[0], s=0.8, color = c )

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


if __name__ == '__main__':
    main()
