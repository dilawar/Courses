"""lottery_up.py: 

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

# N people with 1 lakh rupee each each with their fav. number.
N, startingCapital = 100, 100000
outcome_ = range( 10 )
print( outcome_ )
people = [ (startingCapital, random.choice( outcome_ ) ) for x in range( N ) ] 

capital_ = [ startingCapital * N ]
fortunes_ = [ ]
meanF, varF = [ ], [ ]

def step( week, price = 5 ):
    global people
    ticketsBought = 2 ** week
    cost = price * ticketsBought 
    winningDigit = random.choice( outcome_ )
    #  print( ' Winning digit this week %d' % winningDigit )
    fortunes = [ ]
    capital = 0
    nWin = 0
    for i, (money, favN) in enumerate( people ):
        if cost > money:
            # He can't play anymore. Roughly bankrupt
            f = 0 #money
        else:
            if winningDigit == favN:
                nWin += 1
                win = ( price * 10 + 1 ) * ticketsBought
            else:
                win = 0
            f = money - cost + win
            people[i] = (f, favN)

        capital += f
        fortunes.append(f)

    meanF.append( np.mean( fortunes ) )
    varF.append( np.std( fortunes ) )
    fortunes_.append( fortunes )
    capital_.append( capital )
    return nWin

def main( ):
    # Lets play for 100 weeks.

    week = 0
    while capital_[-1] > 0 and week < 200:
        print( 'Week %d' % week )
        nWin = step( week )
        print( 'Total wins %d, amount: %d' % (nWin, capital_[-1] ) )
        week += 1

    if capital_[-1] == 0:
        print( 'Everyone bankrupt after %d weeks' % week )

    plt.subplot( 211 )
    plt.errorbar( np.arange(0, len(meanF) ), meanF, yerr = varF )
    plt.xlabel( 'Week' )
    plt.ylabel( 'Total capital for all people' )
    plt.subplot( 212 )
    plt.imshow( np.matrix(fortunes_), cmap='seismic', interpolation = 'none'
            , aspect = 'auto' )
    plt.xlabel( 'Fortune of 100 people' )
    plt.ylabel( 'Weeks' )
    plt.colorbar( )
    plt.tight_layout( )

    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()

