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
import ajgar
from collections import defaultdict

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
    res = [ ]
    vs = [0.01, 0.05, 0.1, 0.3, 0.5, 0.9, 1.0 ]
    for i, (I, alpha) in enumerate( itertools.product(vs, repeat=2)):
        system = build_system( I, alpha )
        for ii in range( 10 ):
            t, ys = get_trajectory( system, [ rnd(), rnd() ] )
            ys[:,0] = ys[:,0] % (2*math.pi)
            T = ajgar.find_period( ys[-400:] )
            if T is not None:
                ts, ps = T 
                print( alpha, I, ts, ps )
                res.append( (alpha, I) )
                break

    print( res )
    xs, ys = zip( *res )
    plt.scatter( xs, ys )
    outfile = '%s.png' % sys.argv[0]
    plt.savefig( outfile )
    print( 'Saved to %s' % outfile )

def test( ):
    a = [ 1, 2, 3, -1, -1, 2, 1, -2, -2, 0, 1, 1 ]
    b = [ 1, 2, 0, -1, 0, 0, -1, 2]
    res = [ (x, change_sign(x)) for x in [a, b ] ]
    print( res )

if __name__ == '__main__':
    #  test( )
    main()
