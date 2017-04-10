"""sol8.1.py: 

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

# Both of these function fits very well with cones behaviour.
def red( x ):
    return np.exp( - 5e-4 * (x-550)**2.0 )

def blue( x ):
    return np.exp( - 20e-4 * (x-450)**2.0 )

xvec = np.linspace( 400, 700, 10000 )
rvec = red( xvec )
bvec = blue( xvec )


plt.figure( )
plt.subplot( 2, 1, 1 )
plt.plot( xvec, bvec, label = 'Blue' )
plt.plot( xvec, rvec, label = 'Red' )
plt.ylabel( 'Response' )
plt.xlabel( 'Wavelength of light' )
plt.legend( )
plt.subplot( 2, 1, 2 )


plt.show( )
