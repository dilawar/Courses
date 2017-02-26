"""snake_and_ladder.py: 

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
import random
import numpy as np
import pylab
from collections import defaultdict

loc_ = 0

cells_ = np.zeros( shape=(100,100) )
rows_ = np.zeros( shape=(10,10) )

def check_for_snakes_and_ladders(n):
    """This method checks for the presence of snakes or ladders in the board"""
    ladders = {1:38,4:14,9:31,21:42,28:84,36:44,51:67,71:91,80:100}
    snakes = {98:78,95:75,93:73,87:24,64:60,62:19,56:53,49:11,48:26,16:6}
    # ladders, snakes = {}, {}
    if ladders.has_key(n):
        # print( "Ladder. Up!" )
        n = ladders[n]
    elif snakes.has_key(n):
        # print( "Snake. Down!" )
        n = snakes[n]
    return n

def roll_dice( ):
    return random.randint(1, 6)

def main( ):
    global loc_
    nGames = 10000
    for i in range( nGames ):
        loc_ = 0
        while True:
            r1, c1 = int(loc_ / 10), loc_
            loc_ += roll_dice( )
            loc_ = check_for_snakes_and_ladders( loc_ )
            r2, c2 = int(loc_ / 10), loc_
            if loc_ < 100:
                cells_[c1,c2] += 1.0
                rows_[r1,r2] += 1.0
            if loc_ > 100:
                print( "Won" )
                loc_ = 0
                break

    pCells = cells_ / cells_.sum( axis = 0 )
    pRows = rows_ / rows_.sum( axis = 0 )
    with np.errstate(divide='ignore', invalid='ignore'):
        pCells = np.true_divide(cells_, cells_.sum( axis = 0 ) )
        pRows = np.true_divide( rows_, rows_.sum( axis = 0 ) )
        pCells = np.nan_to_num( pCells )
        pRows = np.nan_to_num( pRows )
    pylab.figure( figsize=(10,6) )
    pylab.subplot( 121 )
    pylab.title( 'Cell to cell transitions' )
    pylab.imshow( pCells, interpolation = 'none' )
    pylab.colorbar( orientation = 'horizontal' )
    pylab.subplot( 122 )
    pylab.imshow( pRows, interpolation = 'none' )
    pylab.colorbar( orientation = 'horizontal' )
    pylab.title( 'Row to row transitions' )
    pylab.suptitle( 'Total games %d' % nGames )
    pylab.tight_layout( )
    pylab.savefig( '%s.png' % sys.argv[0] )
    

if __name__ == '__main__':
    main()
