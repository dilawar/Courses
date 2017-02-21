"""uniform_gauss.py: 

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
from collections import Counter
import random

# Let me define coin flips.
HEAD, TAIL = '1', '0'

def bits2Num( bits ):
    """This is the standard way, you convert boolean to decimal.
    """
    num = 0
    for i, x in enumerate( bits ):
        if x == HEAD:
            num += 2 ** i
    return num

def stepsToLocation( bits ):
    loc = 0
    for x in bits:
        if x == TAIL:
            # go negative
            loc -= 1
        else:
            loc += 1
    return loc

def main( n = 1000 ):
    results = [ ]
    # Toss the coin 10 times and do it n times.
    for i in range( n ):
        thisToss = ''
        for j in range( 10 ):
            thisToss += random.choice( [ HEAD, TAIL ] )
        results.append( thisToss )

    # Now the claim is that this distribution will be uniform for large enough n
    # but first we must convert these string of 0 and 1 to numbers.

    # Applying bits2Num function to each element
    numbers =  map( bits2Num, results)

    # How to get Gaussian? The analogy of random walk. Lets count the number of
    # 1 in string. Thats akin to steps taken to right side (or left). So mean
    # position from 0 will be steps to right - steps to left. Or count of 1 -
    # count of 0. We write a function stepsToLocation and apply it to the
    # bits collected in results
    location = map( stepsToLocation, results )

    # Now we plot histogram
    plt.figure( )
    plt.subplot( 2, 1, 1 )
    plt.hist( numbers, label = "Is this uniform?" )
    plt.legend( )
    plt.subplot( 2, 1, 2 )
    plt.hist( location, bins = 1 + max(location), label = "Is this Gaussian?" )
    plt.legend( )
    plt.suptitle( "Number of samples %d" % n )
    plt.show( )


if __name__ == '__main__':
    main( n = 10 ** 5 )
