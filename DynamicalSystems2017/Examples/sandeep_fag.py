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
import numpy as np
import scipy.integrate

def plot( ts, ys, outfile ):
    plt.plot( ts, ys[:,0], label = 'x' )
    plt.plot( ts, ys[:,1], label = 'y' )
    plt.legend( )
    plt.savefig( outfile )
    print( '[INFO] Saved to %s' % outfile )

def system(ss, t):
    x, y = ss
    return [ x*(1-x)-x*y, 100*x*y - 0.1*y ]

def lsoda( ts, init ):
    solver_ = 'lsoda'
    ys = scipy.integrate.odeint( system, init, ts )
    plot( ts, ys, '%s_%s.png' % ( sys.argv[0], solver_ ) )

def main( ):
    init = [100, 50]
    ts = np.arange( 0, 1000, 0.1 )
    lsoda( ts, init )

if __name__ == '__main__':
    main()
