"""sol3.py: 

Quadratic approximation.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2016, Dilawar Singh"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import matplotlib.pyplot as plt
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt
try:
    mpl.style.use( 'seaborn-talk' )
except Exception as e:
    pass
mpl.rcParams['axes.linewidth'] = 0.1
plt.rc('text', usetex=True)
plt.rc('font', family='serif')


def plot_approximation(ax, f, at ):
    xs = np.linspace( at - 0.1, at + 0.1, 10000 )
    ax.plot( xs, f(xs) )
    ys = f( xs )
    newX, newY = [], []
    for h in xs - at:
        y0 = ys[ len(ys) / 2 ]
        fd = np.diff( [ f(at), f(at+1e-6), f(at+2e-6) ] )
        fdd = np.diff( fd ) 
        fd = fd[0] / 1e-6 
        fdd = fdd[0] / 1e-6
        newX.append( at + h )
        newY.append( f(at) + h * fd + (h * h * 0.5 ) * fdd )
    ax.plot( newX, newY, label = 'approximated' )
    ax.legend( framealpha = 0.4 )


def main( ):
    f1 = np.exp
    f2 = np.sin
    f3 = lambda x: x ** 3.0
    f4 = lambda x: 1 / (1 - x )

    gridSize = (2, 2)
    ax1 = plt.subplot2grid( gridSize, (0,0), colspan = 1 )
    ax2 = plt.subplot2grid( gridSize, (0,1), colspan = 1 )
    ax3 = plt.subplot2grid( gridSize, (1,0), colspan = 1 )
    ax4 = plt.subplot2grid( gridSize, (1,1), colspan = 1 )
    plot_approximation( ax1, f1, at = 0 )
    plot_approximation( ax2, f2, at = 0 )
    plot_approximation( ax3, f3, at = 1 )
    plot_approximation( ax4, f4, at = 0 )
    plt.suptitle( 'Quadratic approximation of various functions' )
    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()
