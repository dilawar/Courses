import sys
import os
import numpy as np
import random

import matplotlib as mpl
import matplotlib.pyplot as plt

def compute_x(a, b, c):
    xs = np.random.uniform( -10*(a+b), 10*(a+b), 100000 )
    res = []
    for x in xs:
        if abs(x-c) > a and abs(x-c) < b:
            res.append( x )
    return res

def main():
    for i in range( 5 ):
        a = random.randint(0, 100)
        b = random.randint(a+1, 10*a)
        c = random.randint(-a, b)
        print(a, b, c)
        res = compute_x(a, b, c )
        plt.subplot(5, 1, i+1)
        plt.plot( res, [1] * len(res), 'o' )
        plt.xticks( [a+c, b+c,-a+c, -b+c], [ 'a+c', 'b+c', '-a+c', '-b+c' ] )
        plt.yticks( [], [] )
        plt.title( 'a=%.1f,b=%.1f,c=%.1f' % (a, b, c) )

    plt.tight_layout( )
    plt.savefig( "%s.png" % sys.argv[0] )
    

if __name__ == '__main__':
    main()
