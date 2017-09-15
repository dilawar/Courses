"""toss.py: 

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

def main( ):
    N = 10**6
    outcome = [ 'H', 'T' ]
    flips = ''.join(np.random.choice( outcome, N ))
    twoH = flips.split( 'HH' )
    ns = map( lambda x: len(x) + 2, twoH )
    print( np.mean( ns ), np.std( ns ) )



if __name__ == '__main__':
    main()
