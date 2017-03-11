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

def renormalize( img ):
    with np.errstate(divide='ignore', invalid='ignore'):
        img = np.true_divide(img, img.sum( axis = 0 ) )
        img = np.nan_to_num( img )
    return img

def drawNow( img, title = 'Image' ):
    import cv2 as cv
    cv.imshow( title, img )
    cv.waitKey( 100 )

def main( ):
    global loc_
    nGames = 20
    prevRows, prevCells = np.zeros( shape=(10,10) ), np.zeros( shape=(100,100))
    cellTransitionProbMean = [ ]
    rowTransitionProbMean = [ ]
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
                print( "Game %d is over" % i )
                loc_ = 0
                thisRows = renormalize( rows_ )
                thisCells = renormalize( cells_ )
                diffRow = thisRows - prevRows
                diffCell = thisCells - prevCells
                rowTransitionProbMean.append( diffRow.mean( ) )
                cellTransitionProbMean.append( diffCell.mean( ) )
                prevRows, prevCells = thisRows, thisCells
                # drawNow( img )
                break
                

    pCells = cells_ / cells_.sum( axis = 0 )
    pRows = rows_ / rows_.sum( axis = 0 )
    with np.errstate(divide='ignore', invalid='ignore'):
        pCells = np.true_divide(cells_, cells_.sum( axis = 0 ) )
        pRows = np.true_divide( rows_, rows_.sum( axis = 0 ) )
        pCells = np.nan_to_num( pCells )
        pRows = np.nan_to_num( pRows )
    pylab.figure( figsize=(10,6) )

    gridSize = (2, 2)
    ax1 = pylab.subplot2grid( gridSize, (0,0), colspan = 1 )
    ax2 = pylab.subplot2grid( gridSize, (0,1), colspan = 1 )
    ax3 = pylab.subplot2grid( gridSize, (1,0), colspan = 2 )
    ax1.set_title( 'Cell to cell transitions' )
    img1 = ax1.imshow( pCells, interpolation = 'none' )
    pylab.colorbar( img1, ax = ax1 ) #orientation = 'horizontal' )
    img2 = ax2.imshow( pRows, interpolation = 'none' )
    pylab.colorbar( img2, ax = ax2 ) # orientation = 'horizontal' )
    ax2.set_title( 'Row to row transitions' )

    ax3.plot( rowTransitionProbMean, '-o', label = 'Row probs' )
    ax3.plot( cellTransitionProbMean, '-*', label = 'Cell probs' )
    ax3.set_ylabel( 'diff of transition probs'  )
    ax3.set_xlabel( 'Number of games completed' )
    ax3.legend( )

    pylab.suptitle( 'Total games %d' % nGames )
    # pylab.tight_layout( )
    pylab.savefig( '%s.png' % sys.argv[0] )
    

if __name__ == '__main__':
    main()
