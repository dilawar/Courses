"""prob2e.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import random
import numpy as np

def using_numpy(n = 250):
    N = 2000000
    found = 0
    for i in range(1, N):
        rolls = np.random.randint(1, 7, n)
        bins = np.bincount(rolls)
        # Bincount also have 0.
        if bins[2] >= 2*bins[1]:
            found += 1
        if i % 100000 == 0:
            print( f"ANS: {found/i:.8f} after {i} trials.")

def main(n=250):
    using_numpy(n)

if __name__ == '__main__':
    main(250)
