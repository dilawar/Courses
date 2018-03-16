#!/usr/bin/env python

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
import random
import matplotlib.pyplot as plt
import numpy as np
import scipy.integrate as sci

def rnd( ):
    return random.random() - 0.5


def pendulum_sympy( ):
    import sympy as sy
    t = sy.Symbol( 't', real = True )
    #  theta, u = sy.symbols( 'theta, u', function = True )
    vars = 'theta'.split( ',' )
    m, L, g, b, gamma = sy.symbols( 'm,L,g,b,Gamma' )
    m, L, g = 1, 1, 10
    gamma = 1
    b = 1
    theta,  = [ sy.Function(x) for x in vars ]
    system = sy.Eq( m*(L**2)*sy.diff(theta(t),t, 2) + \
            b * theta(t).diff(t) + \
            m*g*L * sy.sin(theta(t)) - \
            gamma
            )
    sy.pprint( system )

def system( y, t, b = 0.25, Gamma=1 ):
    m, g, L = 1, 10, 1
    theta, u = y
    return [u, (-b*u-m*g*L*np.sin(theta)+Gamma)/m/L/L ]

def pendulum_scipy( ):
    m, L, g, gamma, b = 1, 1, 10, 1, 1
    ts = np.linspace( 0, 20, 1000 )
    for i in range( 100 ):
        res = sci.odeint( system, [np.pi * rnd(), rnd()], ts )
        plt.plot( ts, res[:,0], label = 'theta' )
    plt.savefig( '%s.png' % sys.argv[0] )
    print( 'Saved to %s.png' % sys.argv[0] )


def main( ):
    sys = pendulum_scipy( )

if __name__ == '__main__':
    #init_session( )
    main()
