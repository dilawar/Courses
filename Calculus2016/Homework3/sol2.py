"""sol2.py: 

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
import matplotlib as mpl
mpl.use( 'TkAgg' )
import matplotlib.pyplot as plt
mpl.rcParams['axes.linewidth'] = 0.1
plt.rc('text', usetex=True)
plt.rc('font', family='serif')

import matplotlib.pyplot as plt
import numpy as np
import math


def plot_2a( ax ):
    xs = np.linspace( -2, 2, 10000 )
    ys = np.abs( xs ** 3.0 ) / xs
    ax.plot( xs, ys )
    ax.set_title( '$f(x)=|x^3|/x$' )
    ax.set_ylim( bottom = -10, top = 10 )


def plot_2b( ax ):
    xs = np.linspace( -2, 2, 10000 )
    ys = 1.0 / (np.abs( xs ) - 1.0)
    ax.plot( xs, ys )
    ax.set_ylim( bottom = -10, top = 10 )
    ax.set_title( '$f(x)=\\frac{1}{|x|-1}$' )


def plot_2c( ax ):
    xs = np.linspace( -2, 2, 10000 )
    ys = 1.0 / (np.abs( xs ) + 1.0)
    ax.plot( xs, ys )
    ax.set_ylim( bottom = -10, top = 10 )
    ax.set_title( '$f(x)=\\frac{1}{|x|+1}$' )

def plot_2d( ax ):
    xs = np.linspace( -2, 2, 10000 )
    ys = 1.0 / np.abs( xs - 1.0 ) 
    ax.set_ylim( bottom = -10, top = 10 )
    ax.plot( xs, ys )
    ax.set_title( '$f(x)=\\frac{1}{|x-1|}$' )

def main( ):
    gridSize = (2, 2)
    ax1 = plt.subplot2grid( gridSize, (0,0), colspan = 1 )
    ax2 = plt.subplot2grid( gridSize, (0,1), colspan = 1 )
    ax3 = plt.subplot2grid( gridSize, (1,0), colspan = 1 )
    ax4 = plt.subplot2grid( gridSize, (1,1), colspan = 1 )

    plot_2a( ax1 )
    plot_2b( ax2 )
    plot_2c( ax3 )
    plot_2d( ax4 )

    plt.tight_layout( )
    plt.savefig( 'solution2.png' )



if __name__ == '__main__':
    main()
