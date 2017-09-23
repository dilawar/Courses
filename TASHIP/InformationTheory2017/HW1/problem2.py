"""problem2.py: 

    Solution to problem2.

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
import random
import numpy as np

def getCoins ( N ):
    goodcoins = [ 2 ] * (N-1)
    counterfiet = random.choice( [1, 3 ] )
    coins = [ counterfiet ] + goodcoins
    #  random.shuffle( coins )
    return coins

def listToStr( vec ):
    x = map( lambda c: '%1d' % c, vec )
    return ' '.join( x )

def step( coins ):
    # First divide coins into three piles and compare any two.
    k = int( (1 + len( coins )) / 3 )
    parts = coins[0:k], coins[k:2*k], coins[2*k:]

    print( '\t' + '\n\t'.join( map(listToStr, parts ) ) )

    if sum( parts[0] ) == sum( parts[1] ):
        # Last part has the counterfiet coin.
        return 1, parts[2]
    else:
        # First two parts have counterfiet coin. Split one of them into two and
        # compare to restrict the options to 1.
        fst, snd = parts[0], parts[1]

        if len( fst ) % 2 == 1:
            fst.append( 2 )   # add a normal coin.

        # Split this into 2 and if both weigh same then otherpartition has the
        # lighter coin, otherwise this partition has the counterfiet coin. We
        # have done one extra weighing.
        nw = len( fst )
        if sum( fst[:nw] ) == sum( fst[nw:] ):
            return 2, snd
        else:
            return 2, fst

def main( ):
    coins = getCoins( 12 )
    print( '0: %s' % coins )

    i = 0
    while True:
        w, coins = step( coins )
        i += w
        print( '%d: %s' % (i, listToStr(coins) ) )
        if len( coins ) <= 2:
            break

    print( 'Total weighings: %d' % i )


if __name__ == '__main__':
    main()
