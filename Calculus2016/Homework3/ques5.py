"""ques5.py: 

Question 5.

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
import math
import matplotlib.pyplot as plt
import numpy as np

def plot_func( ax, f ):
    xs = np.linspace( -11, 10, 10**5 )
    ax.plot( xs, f(xs) )

def main( ):
    gridSize = (4, 1)
    ax1 = plt.subplot2grid( gridSize, (0,0), colspan = 1 )
    ax2 = plt.subplot2grid( gridSize, (1,0), colspan = 1 )
    ax3 = plt.subplot2grid( gridSize, (2,0), colspan = 1 )
    ax4 = plt.subplot2grid( gridSize, (3,0), colspan = 1 )
    plot_func( ax1, lambda x: x**4 - 2*(x**2.0) + 3 )
    plot_func( ax2, lambda x: (x**2.0 + 2 )/(x-3) )
    plot_func( ax3, lambda x: np.sin( x ) ** 2.0 )
    plot_func( ax4, lambda x: ( x * x + x ) ** 0.5 )
    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()
