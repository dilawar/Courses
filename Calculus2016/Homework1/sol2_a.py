import sys
import os
import numpy as np
import random

import matplotlib as mpl
import matplotlib.pyplot as plt

def main():
    xs = np.random.randint(-10000, 10000, 10**5)
    ys = np.random.randint(-10000, 10000, 10**5)
    v1 = np.abs( np.abs(xs) - np.abs( ys ) )
    v2 = np.abs( xs + ys )
    plt.plot( v1, v2,  '.' )
    plt.xlabel( '||x| - |y||' )
    plt.ylabel( '|x + y|' )
    plt.tight_layout( )
    plt.savefig( "%s.png" % sys.argv[0] )
    

if __name__ == '__main__':
    main()
