"""prob2.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import itertools
import random
import operator
from functools import reduce
from collections import Counter

dist_ = { 'A' : 1/2.0, 'T' : 1/4.0, 'G' : 1/8.0, 'C' : 1/8.0 }
vars_ = dist_.keys( )

def seqToStr( seq ):
    return ''.join( seq )

def seq_prob( seq ):
    prob = reduce( operator.mul, [ dist_[x] for x in seq ] )
    return prob

def is_typical( seq ):
    """In a typical seq, letters occur as expected
    """
    global dist_
    dist = Counter( seq )
    isTypical = True
    for v in vars_:
        if dist.get(v, 0.0) / len(seq) != dist_[v]:
            isTypical = False
    return isTypical

def typical( seqs ):
    seqWithProbs = [ ]
    typicalSeqs = [ ]
    for s in seqs:
        seqWithProbs.append( (seq_prob(s), s ) )
        if is_typical( s ):
            typicalSeqs.append( s )

    seqWithProbs.sort( reverse = True )
    print( 'Singal most probable seq        : %s' % seqToStr(seqWithProbs[0][1]) )

    typProb = 0.0
    if len( typicalSeqs ) > 0:
        typProb = seq_prob( typicalSeqs[0] )

    print( 'Probability of a typical seq    : %f' % typProb )
    print( 'Number of typical seqs          : %d' % len( typicalSeqs) )
    print( 'Probability of getting typical  seq: %f' % ( 
        len( typicalSeqs ) / len( seqs ))
        )

def solve( size ):
    seqs = list( itertools.product( vars_ , repeat = size ) )
    print( '[INFO] Total sequences (size=%d): %d' % (size, len( seqs )) )
    typical( seqs )

def main( ):
    for size in [ 4, 6, 8, 10]: #, 12, 14 ]:
        solve( size )
        print( "" )


if __name__ == '__main__':
    main()
