"""solve_problem2.py: 

    Assignment 1, Problem 2
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

pylab.style.use('ggplot')

def prob( t, y, k):
    return  k * y

def system( k, init):
    r = spi.ode( prob )
    # y at value t
    print('Building system for k=%s' % k)
    print('\tSetting y(%s) = %s' % (init[1], init[0] ))
    r.set_integrator( 'zvode' )
    r.set_initial_value(init[0], init[1])
    r.set_f_params( k )
    return r

def main():
    t1 = 10
    dt = 0.01
    pylab.figure( figsize = (15, 10) )
    valuesAt10 = defaultdict(list)
    for i, k in enumerate([ 5, -3, 12, -1.5]):
        for j, init in enumerate([ (1,0), (1,1), (1,-1), (-1,-1) ]):
            r = system( k, init)
            result, tvec = [], []
            while r.successful() and r.t < t1:
                result.append( r.integrate( r.t + dt ) )
                tvec.append( r.t )
            pylab.subplot(4, 4, 4*i + j + 1)
            pylab.plot( tvec, result )
            # pylab.yscale( 'symlog' )
            title = 'k=%s,(y,t)=%s' % (k, init)
            pylab.title( title )
            valuesAt10[k].append( (init, result[-1]) )
    for case in valuesAt10:
        print('Case :%s' % case)
        for i, v in valuesAt10[case]:
            print('\t Init %10s  y(10) = %s' % (i, v[0]))
    pylab.tight_layout( )
    pylab.savefig( '%s.png' % sys.argv[0] )


if __name__ == '__main__':
    main()
