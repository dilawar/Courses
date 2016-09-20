import sys
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np

def main( ):
    for i in range( 4 ):
        plt.subplot( 4, 1, i + 1)
        a = 2 ** (i + 2)
        xs = np.random.random( 10000 )
        xs = 2 * a * (xs - 0.5 )
        res = []
        for x in xs:
            v = abs( (x**2.0) - 2.0 * x - 3.0)
            if v > x:
                res.append( x )
        plt.plot( res, [1]*len(res), '|', label = '-%.1f < x < %.1f' % (a,a) )
        plt.legend( framealpha = 0.4 )
        plt.yticks( [], [] )
        plt.xlabel( 'x' )
    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()
