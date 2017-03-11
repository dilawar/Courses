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
    return w.lower( )

def main( ):
    with open( './war_and_peace.txt', 'r' ) as f:
        txt = f.read( )

    words = list( filter( fix, txt.split( ) ) )
    print( 'Total words %d' % len( words ) )
    nWords = len( set(words) )
    print( 'Total unique words %d' % nWords )

    img = np.zeros( shape=(nWords,nWords) )
    for i, w in enumerate( words[1:] ):
        prevW = words[ i ]
        transitions_[ '%s:%s' % (prevW.lower(), w.lower()) ] += 1

    print( 'Counted all transitions' )
    for pair in sorted( transitions_ ):
        try:
            indices = map( lambda x : words.index( x ), pair.split( ':' ) )
            if len( indices ) == 2:
                x, y = indices 
                img[x,y] = transitions_[ pair ]
        except Exception as e:
            print( '.', end = '' )
            pass

    pyplot.imshow( img, interpolation = 'none' )
    pyplot.show( )

if __name__ == '__main__':
    main()
