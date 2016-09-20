import sys
import os
import numpy as np
import random

import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def main():
    fig = plt.figure()
    ax = plt.axes( projection = '3d' )
    xs = np.random.randint(-10000, 10000, 10**3)
    ys = np.random.randint(-10000, 10000, 10**3)
    v1 = np.abs( np.abs(xs) - np.abs( ys ) )
    v2 = np.abs( xs + ys )
    ax.scatter( xs, ys, v1, marker ='.' )
    # ax.plot_wireframe( xs, ys, v1, color ='red' )
    # ax.plot_trisurf( xs, ys, v1, color ='red', alpha = 0.4 )
    ax.scatter( xs, ys, v2, marker = ',' )
    # ax.plot_wireframe( xs, ys, v2, color = 'blue' )
    # ax.plot_trisurf( xs, ys, v2, color = 'blue', alpha = 0.4 )
    plt.xlabel( '||x| - |y||' )
    plt.ylabel( '|x + y|' )
    plt.tight_layout( )
    plt.savefig( "%s.png" % sys.argv[0] )
    # plt.savefig( "%s_3d.png" % sys.argv[0] )
    

if __name__ == '__main__':
    main()
