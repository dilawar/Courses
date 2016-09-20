import sys
import os
import numpy as np
import random

import matplotlib as mpl
import matplotlib.pyplot as plt

def compute_x(a, b):
    xs = np.random.uniform( -10*(a+b), 10*(a+b), 100000 )
    res = []
    for x in xs:
        if abs(x - (a+b)/2.0) < (b-a)/2.0:
            res.append( x )
    return res

def main():
    for i in range( 5 ):
        a = random.randint(0, 100)
        b = random.randint(a+1, 10*a)
        res = compute_x(a, b)
        plt.subplot(5, 1, i+1)
        plt.plot( res, [1] * len(res), 'o' )
        plt.xticks( [a, b], [ 'a', 'b' ] )
        plt.yticks( [], [] )
        plt.title( 'a=%.1f,b=%.1f' % (a, b) )

    plt.tight_layout( )
    plt.savefig( "%s.png" % sys.argv[0] )
    

if __name__ == '__main__':
    main()
