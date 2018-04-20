#!/usr/bin/env python3
"""analyze.py:

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import matplotlib.pyplot as plt
import numpy as np

def main():
    data = np.loadtxt( sys.argv[1] )
    data = data.T
    for i, r1 in enumerate(data):
        for r2 in data[i+1:,:]:
            if (r1 == r2).all():
                print('Same' )

    plt.subplot(211)
    plt.imshow( data, interpolation = 'none', aspect = 'auto' )
    plt.savefig( '%s.png' % sys.argv[1] )

if __name__ == '__main__':
    main()
