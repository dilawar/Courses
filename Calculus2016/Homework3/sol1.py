"""sol1.py: 

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
try:
    mpl.style.use( 'ggplot' )
except Exception as e:
    pass
mpl.rcParams['axes.linewidth'] = 0.1
plt.rc('text', usetex=True)
plt.rc('font', family='serif')

import matplotlib.pyplot as plt
import numpy as np
import math

def fun1( xs ):
    res =  []
    k = 4
    for x in xs:
        if x >= -3 and x <= -1:
            y = 1.0
        elif x > -1 and x < 0:
            y = - math.exp( - k * x ) / math.exp( k )
        elif x > 0 and x <= 3:
            y = x * ( 3.0 - x) / (1.5 * 1.5 )
        else:
            y = -1000.0
            print( 'Undefined for this value %f' % x )
        res.append( y )
    return np.array(res)

def fun2( xs ):
    ys = []
    for x in xs:
        if x >= -2 and x < -1:
            y = 2.0
        elif x >= -1 and x < 1:
            y = 1.0 - x
        elif x >= 1 and x <= 2:
            y = 2 * (x - 1) ** 0.5
        else:
            y = -1000.0
            print( 'Undefined for this value of x %f' % x )
        ys.append( y )
    return np.array(ys)


def plot_reference_plot( ax1, ax2 ):
    x = np.linspace(-3, 3, 1000 )
    y = fun1( x )
    ax1.plot( x, y, 'o' )
    ax1.set_ylim( (-1.1, 1.1) )
    ax1.set_title( 'fun1' )

    xs = np.linspace( -2, 2, 1000 )
    ys = fun2( xs )
    ax2.plot( xs, ys, 'o' )
    ax2.set_ylim( (-0.1, 2.1 ) )
    ax2.set_title( 'fun2' )


def plot_1a( ax1, ax2 ):
    xs = np.linspace( -5, 5, 10000 )
    ys1 = fun1( np.abs( xs - 1 ) )
    ys2 = fun2( np.abs( xs - 1) )
    ax1.plot( xs, ys1, ',' )
    ax1.set_ylim( bottom = -1, top = max(ys1) + 1 )
    ax1.set_title( '$fun1(|x -1|)$' )

    ax2.plot( xs, ys2, ',' )
    ax2.set_ylim( bottom = -1, top = max(ys2) + 1 )
    ax2.set_title( '$fun2(|x -1|)$' )


def plot_1b( ax1, ax2 ):
    xs = np.linspace( -5, 5, 10000 )
    ys1 = fun1( np.abs( xs )- 1 )
    ys2 = fun2( np.abs( xs ) - 1 )
    ax1.plot( xs, ys1, ',' )
    ax1.set_ylim( bottom = -1, top = max(ys1) + 1 )
    ax1.set_title( '$fun1(|x| -1)$' )

    ax2.plot( xs, ys2, ',' )
    ax2.set_ylim( bottom = -1, top = max(ys2) + 1 )
    ax2.set_title( '$fun2(|x| -1)$' )


def plot_1c( ax1, ax2 ):
    xs = np.linspace( -5, 5, 10000 )
    ys1 = fun1( np.abs( xs ) + 1 )
    ys2 = fun2( np.abs( xs ) + 1 )
    ax1.plot( xs, ys1, ',' )
    ax1.set_ylim( bottom = -1, top = max(ys1) + 1 )
    ax1.set_title( '$fun1(|x| +1)$' )

    ax2.plot( xs, ys2, ',' )
    ax2.set_ylim( bottom = -1, top = max(ys2) + 1 )
    ax2.set_title( '$fun2(|x| + 1)$' )
    

def plot_1d( ax1, ax2 ):
    xs = np.linspace( -5, 5, 10000 )
    ys1 = fun1( 0.5 * xs + 1 )
    ys2 = fun2( 0.5 * xs + 1 )
    ax1.plot( xs, ys1, ',' )
    ax1.set_ylim( bottom = -1, top = max(ys1) + 1 )
    ax1.set_title( '$fun1( x/2 +1)$' )

    ax2.plot( xs, ys2, ',' )
    ax2.set_ylim( bottom = -1, top = max(ys2) + 1 )
    ax2.set_title( '$fun2( x/2 + 1)$' )

def plot_1e( ax1, ax2 ):
    xs = np.linspace( -5, 5, 10000 )
    ys1 = 1.0 / fun1( xs )
    ys2 = 1.0 / fun2( xs )
    ax1.plot( xs, ys1, ',' )
    ax1.set_ylim( bottom = 0, top = max(ys1) + 1 )
    ax1.set_title( '$1/fun1$' )

    ax2.plot( xs, ys2, ',' )
    ax2.set_ylim( bottom = 0, top = max(ys2) + 1 )
    ax2.set_title( '$1/fun2$' )
    

def main( ):
    gridSize = (6, 2)
    plt.figure( figsize=(14,10) )
    ax1 = plt.subplot2grid( gridSize, (0,0), colspan = 1 )
    ax2 = plt.subplot2grid( gridSize, (0,1), colspan = 1 )
    ax3 = plt.subplot2grid( gridSize, (1,0), colspan = 1 )
    ax4 = plt.subplot2grid( gridSize, (1,1), colspan = 1 )
    ax5 = plt.subplot2grid( gridSize, (2,0), colspan = 1 )
    ax6 = plt.subplot2grid( gridSize, (2,1), colspan = 1 )
    ax7 = plt.subplot2grid( gridSize, (3,0), colspan = 1 )
    ax8 = plt.subplot2grid( gridSize, (3,1), colspan = 1 )
    ax9 = plt.subplot2grid( gridSize, (4,0), colspan = 1 )
    ax10 = plt.subplot2grid( gridSize, (4,1), colspan = 1 )
    ax11 = plt.subplot2grid( gridSize, (5,0), colspan = 1 )
    ax12 = plt.subplot2grid( gridSize, (5,1), colspan = 1 )

    plot_reference_plot( ax1, ax2 )
    plot_1a( ax3, ax4 )
    plot_1b( ax5, ax6 )
    plot_1c( ax7, ax8 )
    plot_1d( ax9, ax10 )
    plot_1e( ax11, ax12 )

    plt.tight_layout( )
    plt.savefig( 'solution1.png' )



if __name__ == '__main__':
    main()
