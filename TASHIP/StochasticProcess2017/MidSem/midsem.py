"""midsem.py: 

War and peace.

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

from collections import defaultdict

transitions_ = defaultdict( int )
words_ = [ ]

def fix( w ):
    w = w.translate( { ord(i) : None for i in ';.[])(!,' } )
    if len( w ) > 1:
        return w.lower( )
    return False

def main( ):
    with open( './war_and_peace.txt', 'r' ) as f:
        txt = f.read( )

    words = list( filter( fix, txt.split( ) ) )
    words_ = list( set( words ) )

    print( 'Total words %d' % len( words ) )
    nWords = len( set(words) )
    img = np.zeros( shape=(nWords,nWords) )
    print( 'Total unique words %d' % nWords )

    for i, w in enumerate( words[1:] ):
        prevW = words[ i ]
        transitions_[ '%s<|>%s' % (prevW, w) ] += 1.0

    print( 'Counted all transitions' )
    for pair in transitions_: 
        x, y = map( lambda x : words_.index( x ), pair.split( '<|>' ) )
        img[x,y] = transitions_[ pair ]

    print( '|- Mean %f, max %f, std %f' % ( img.mean(), img.max(), img.std() ) )
    plt.imshow( img[::20,::20], interpolation = 'none' )
    plt.show( )

if __name__ == '__main__':
    main()
