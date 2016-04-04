"""solve_problem8.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2015, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import numpy as np
import pylab
from scipy import interpolate
import scipy.integrate as spi
import sys

def system(prob, init ):
    # Build system using prob with initial conditions. 
    r = spi.ode( prob )
    # y at value t
    print('\tSetting x(%s) = %s' % (init[1], init[0] ))
    r.set_integrator( 'lsoda' )
    r.set_initial_value(init[0], init[1])
    return r

# Following functions recreate the graph shown in given image, approximately
def graph_a( t ):
    tt = range(-3, 8)
    ys = [0, 0.4, 0.7, 0.8, 0.6, 0, -0.5, -0.8, -1.0, -1.2, -1.35 ]
    assert len(tt) == len(ys), (len(tt), len(ys))
    s = interpolate.InterpolatedUnivariateSpline(tt, ys)
    return s(t)


def graph_b( t ):
    tt = range(-4, 8)
    ys = [ 1.0, 0.6, 0, -0.5, 0, 0.4, 0.5, 0.55, 0.57, 0.58, 0.59, 0.6 ]
    assert len(tt) == len(ys), (len(tt), len(ys))
    s = interpolate.InterpolatedUnivariateSpline(tt, ys)
    return s(t)

def graph_c( t ):
    tt = range(-4, 8)
    ys = [ 0.6, 0.45, 0, -0.4, -0.5, -0.4, 0, 0.4, 0, -0.32, -0.42, -0.42 ]
    assert len(tt) == len(ys), (len(tt), len(ys))
    s = interpolate.InterpolatedUnivariateSpline(tt, ys)
    return s(t)

def graph_d( t ):
    tt = [-4, -3.5, -3, -2.5, -2.0, -1, 0, 1, 2, 3, 4, 5, 6, 7 ]
    ys = [ -0.4, -0.25, 0, 0.3, 0, -0.5, 0, 0.4, 0.6, 0, -0.32, -0.42, -0.42, -0.42 ]
    assert len(tt) == len(ys), (len(tt), len(ys))
    s = interpolate.InterpolatedUnivariateSpline(tt, ys)
    return s(t)

def main( ):
    t1 = 6
    t2 = -6
    dt = 0.01
    xs = np.linspace(-4, 7, 100)
    graphs = ( graph_a, graph_b, graph_c, graph_d )
    for i, f in enumerate( graphs ):
        pylab.subplot(4, 2, i*2+1)
        pylab.plot(xs,  f( xs ), label = '%s' % i)
        pylab.grid()
        # pylab.legend(loc='best', framealpha=0.4)
        init = (-4, 0)
        r = system( f, init )

        postRes, postT = [], []
        while r.successful() and r.t < t1:
            postRes.append( r.integrate( r.t + dt ) )
            postT.append( r.t )

        # recreate the system to compute before the initial condition (in
        # reverse time).
        r = system( f, init )
        preRes, preT = [], []
        while r.successful() and r.t > t2:
            preRes.append( r.integrate( r.t - dt ) )
            preT.append( r.t )

        preT.reverse(), preRes.reverse()
        tvec = preT + postT
        result = preRes + postRes
        pylab.subplot(4, 2, i*2+2)
        pylab.plot( tvec, result )
        pylab.grid() 
        title = '(x,t)=(%s, %s)' % init
        pylab.title( title )
    pylab.tight_layout( )
    pylab.savefig( '%s.png' % sys.argv[0].replace('.', '_') )


if __name__ == '__main__':
    main()
