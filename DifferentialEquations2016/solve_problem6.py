"""solve_problem6.py: 

    Assignment 1, Problem 6.
"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2016, Dilawar Singh "
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import numpy as np
import scipy as sp
import scipy.integrate as spi
import pylab
import sys
from collections import defaultdict

pylab.style.use('seaborn-whitegrid')

def prob( t, p):
    return  1.0 - np.exp(p)

def system( init ):
    # Build system using prob with initial conditions. 
    r = spi.ode( prob )
    # y at value t
    print('\tSetting x(%s) = %s' % (init[1], init[0] ))
    r.set_integrator( 'lsode' )
    r.set_initial_value(init[0], init[1])
    return r

def main():
    t1 = 10
    dt = 0.01
    # pylab.figure( figsize = (15, 10) )
    for j, init in enumerate([ (-1,0), (1, 0), (4, 0) ] ):
        r = system(init)
        result, tvec = [], []
        while r.successful() and r.t < t1:
            result.append( r.integrate( r.t + dt ) )
            tvec.append( r.t )
        pylab.subplot(3, 1, j+1)
        pylab.plot( tvec, result )
        # pylab.yscale( 'symlog' )
        title = '(x,t)=(%s, %s)' % init
        pylab.title( title )
    # pylab.tight_layout( )
    # pylab.show()
    pylab.savefig( '%s.png' % sys.argv[0].replace('.', '_') )


if __name__ == '__main__':
    main()
