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

def step( week ):
    global people
    ticketsBought = 2 ** week
    cost = 5 * ticketsBought 
    winningDigit = random.choice( outcome_ )
    #  print( ' Winning digit this week %d' % winningDigit )
    fortunes = [ ]
    capital = 0
    nWin = 0
    for i, (money, favN) in enumerate( people ):
        if cost > money:
            # He can't play anymore.
            money = 0
            f = 0
        else:
            # Definately remove cost 
            if winningDigit == favN:
                nWin += 1
                win = 55 * ticketsBought
            else:
                win = 0
            f = max(0, money - cost + win)
            people[i] = (f, favN)

        capital += int(f)
        fortunes.append(f)

    meanF.append( np.mean( fortunes ) )
    varF.append( np.std( fortunes ) )
    fortunes_.append( fortunes )
    capital_.append( capital )
    return nWin

def main( ):
    # Lets play for 100 weeks.

    week = 0
    while capital_[-1] > 0:
        print( 'Week %d' % week )
        nWin = step( week )
        print( 'Total wins %d' % nWin )
        week += 1
    print( 'Everyone bankrupt after %d weeks' % week )
    plt.subplot( 211 )
    plt.errorbar( np.arange(0, len(meanF) ), meanF, yerr = varF )
    plt.subplot( 212 )
    print( fortunes_) 
    plt.imshow( np.matrix(fortunes_), cmap='seismic', interpolation = 'none', aspect = 'auto' )
    plt.colorbar( )

    plt.savefig( '%s.png' % sys.argv[0] )

if __name__ == '__main__':
    main()

